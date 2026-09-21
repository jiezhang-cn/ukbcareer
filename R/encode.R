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
    stop("Tier 2 requires torch: install.packages(\"torch\"); ",
         "torch::install_torch()\n",
         "  Tier 0 (work history, nine states, exposure, CAMSIS) and Tier 1 (mobility geometry) do not need it.")
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


#' 冻结表征（latent）
#'
#' 对用户样本跑一次前向，得到与论文同一个 192 维空间里的坐标。
#' 全链条无随机性 —— 同一份输入必须逐位复现。
#'
#' **绝不允许一个性别的样本过另一个性别的模型**：表征是分性别独立训练的
#' （女性生涯结构不同，且 CAMSIS 量表本身性别特异）。本函数按 `sex` 分批，
#' 各过自己的模型再合并。
#'
#' @param x [ukb_worklife] 的返回值。
#' @param bundle [ukb_bundle] 的返回值（需要 Tier 2）。
#' @param batch_size 批大小。CPU 上 512 是个稳妥值。
#' @param verbose 打印进度与表征的基本诊断。
#' @return 在 `x` 上加一项 `latent`：`eid` / `sex` / `z001`…`z192`。
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
    stop("The bundle has no encoder_*.ts -> Tier 2 unavailable")
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
