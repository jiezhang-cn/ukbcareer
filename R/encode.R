# ══════════════════════════════════════════════════════════════════
# Tier 2：TorchScript 前向 → 冻结的 192 维表征
# ══════════════════════════════════════════════════════════════════
# wrapper 由 `codes/py/07_export_bundle.py` 导出，固定 **12 个位置参数**：
#   soc, gap, age, dt, n_jobs, is_transition, is_parallel, intens,
#   soc_l1, soc_l2, soc_l3, pad
# **顺序即契约。** 导出时已逐值校验过 TorchScript 与 Python 的 represent 一致
# （实测 max|Δ| = 0）。

#' TorchScript wrapper 的通道顺序
#' @noRd
TS_CHANNELS <- c("soc", "gap", "age", "dt", "n_jobs", "is_transition",
                 "is_parallel", "intens", "soc_l1", "soc_l2", "soc_l3", "pad")


#' 载入 TorchScript 编码器
#' @noRd
.load_encoder <- function(bundle, sex) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop("Encoding (ukb_encode) requires the torch package: ",
         "install.packages(\"torch\"); torch::install_torch()\n",
         "  Everything computed from your CSVs alone (work history, annual state ",
         "sequences, exposure, CAMSIS) and the mobility measures (ukb_movement) ",
         "do not need it.")
  }
  f <- .encoder_path(bundle, sex)
  m <- torch::jit_load(f)
  # ScriptModule 在 R torch 里不接受 `$eval()`（会被当成属性赋值而报
  # "unused argument"）。不需要调它：Python 侧导出前已 `.eval()`，保存下来的
  # 模块 training 标志就是 FALSE，dropout 已关。这一点由"跑两次逐位相同"的
  # 确定性检查兜底（见 tests/testthat/test-encode.R）。
  try(m$eval(), silent = TRUE)
  meta_f <- file.path(bundle$path, sprintf("encoder_%s.meta.json", sex))
  attr(m, "meta") <- .read_json(meta_f)
  m
}


#' Encode careers into the frozen 192-dimensional representation
#'
#' Runs your sample once through the pre-trained career encoder, giving each
#' person coordinates in the same 192-dimensional space as the paper. The
#' whole pipeline is deterministic: the same input always reproduces
#' bit-identical output. Requires the model bundle (with its encoder,
#' `encoder_*.ts`) and the torch package.
#'
#' **People are never passed through the other sex's model.** The
#' representations were trained separately for each sex (women's careers are
#' structured differently, and the CAMSIS scale itself is sex-specific), so
#' this function splits the sample by `sex`, runs each part through its own
#' model, and then combines the results.
#'
#' With `verbose = TRUE` it also prints the effective rank of the
#' representation (about 33 in the reference cohort; a value in single
#' digits suggests collapse, while a somewhat lower value in a much smaller
#' sample is just a sample-size effect).
#'
#' @param x Output of [ukb_worklife].
#' @param bundle Output of [ukb_bundle]; it must contain the encoder.
#' @param batch_size Batch size for the forward pass. 512 is a safe choice on
#'   a CPU.
#' @param verbose If `TRUE`, print progress and basic diagnostics of the
#'   representation.
#' @return `x` with an added element `latent`: columns `eid`, `sex` and
#'   `z001` ... `z192`.
#' @examples
#' \dontrun{
#' wl <- ukb_encode(wl, bundle = b)
#' dim(wl$latent)
#' }
#' @export
ukb_encode <- function(x, bundle, batch_size = 512L, verbose = TRUE) {
  stopifnot(inherits(x, "ukbcareer_worklife"),
            inherits(bundle, "ukbcareer_bundle"))
  if (!isTRUE(bundle$tiers[["tier2"]])) {
    stop("Encoding unavailable: the bundle lacks encoder_*.ts (the model encoder)")
  }
  max_len <- as.integer(bundle$manifest$max_len %||% DEFAULTS$max_len)
  out <- list()
  for (sx in levels(x$cohort$sex)) {
    if (!any(x$cohort$sex == sx)) next
    tok <- .tokenize(x, bundle, sx, max_len, verbose)
    m <- .load_encoder(bundle, sx)
    z <- .forward_batched(m, tok, batch_size, verbose)
    d <- data.table::as.data.table(z)
    data.table::setnames(d, sprintf("z%03d", seq_len(ncol(z))))
    d[, `:=`(eid = attr(tok, "eid"),
             sex = factor(sx, levels = levels(x$cohort$sex)))]
    data.table::setcolorder(d, c("eid", "sex"))
    out[[sx]] <- d
    if (verbose) {
      # 有效秩：奇异值谱的参与比。表征塌缩时它会掉到个位数。
      # 参考队列实测约 33 —— 用户样本小得多时这个数会偏低，那是样本量效应。
      zz <- scale(z, scale = FALSE)
      sv <- svd(zz, nu = 0, nv = 0)$d
      er <- exp(-sum((sv^2 / sum(sv^2)) * log(sv^2 / sum(sv^2))))
      message(sprintf("  [%s] latent %d x %d, effective rank %.1f (about 33 in the reference cohort)",
                      sx, nrow(z), ncol(z), er))
    }
  }
  lat <- data.table::rbindlist(out, use.names = TRUE)
  at <- attr(x, "ukbcareer")
  at$model_sha256 <- bundle$manifest$model_sha256
  at$bundle_version <- bundle$version
  attr(x, "ukbcareer") <- at
  x$latent <- lat[]
  x
}


#' 分批前向
#' @noRd
.forward_batched <- function(m, tok, batch_size, verbose = TRUE) {
  n <- nrow(tok$soc)
  bs <- max(1L, as.integer(batch_size))
  z <- NULL
  starts <- seq(1L, n, by = bs)
  for (k in seq_along(starts)) {
    i <- starts[k]
    j <- min(i + bs - 1L, n)
    args <- .to_torch(tok, i:j)
    r <- torch::with_no_grad(do.call(m$represent, args))
    zb <- as.matrix(r)
    z <- if (is.null(z)) {
      matrix(NA_real_, nrow = n, ncol = ncol(zb))
    } else {
      z
    }
    z[i:j, ] <- zb
    if (verbose && length(starts) > 4L && k %% max(1L, length(starts) %/% 4L) == 0L) {
      message(sprintf("    forward pass %d/%d", j, n))
    }
  }
  z
}


#' R 矩阵 → torch 张量（dtype 必须与 Python 侧一致）
#'
#' 整数通道用 int64（embedding 的索引），浮点通道用 float32，`pad` 用 bool。
#' **dtype 错了不会报错，只会给出一个看起来合法的错表征。**
#' @noRd
.to_torch <- function(tok, rows) {
  long_ch <- c("soc", "gap", "age", "soc_l1", "soc_l2", "soc_l3")
  args <- list()
  for (ch in TS_CHANNELS) {
    v <- tok[[ch]]
    if (ch == "intens") {
      sub <- v[rows, , , drop = FALSE]
      args[[ch]] <- torch::torch_tensor(sub, dtype = torch::torch_float())
    } else if (ch == "pad") {
      sub <- v[rows, , drop = FALSE]
      args[[ch]] <- torch::torch_tensor(sub, dtype = torch::torch_bool())
    } else if (ch %in% long_ch) {
      sub <- v[rows, , drop = FALSE]
      storage.mode(sub) <- "integer"
      args[[ch]] <- torch::torch_tensor(sub, dtype = torch::torch_long())
    } else {
      sub <- v[rows, , drop = FALSE]
      args[[ch]] <- torch::torch_tensor(sub, dtype = torch::torch_float())
    }
  }
  args
}
