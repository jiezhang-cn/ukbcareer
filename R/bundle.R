# ══════════════════════════════════════════════════════════════════
# bundle：冻结参数与参考量的载入
# ══════════════════════════════════════════════════════════════════
# bundle 是 `codes/py/07_export_bundle.py` 的产物，约 27 MB：
#
#   Tier 1  soc_embed_{sex}.parquet   353 × 192 的职业码嵌入（+ 2D 坐标）
#   Tier 2  encoder_{sex}.ts          TorchScript 编码器
#   Tier 0  vocab.json / reference_qc.json / reliability.json / tables/
#           type_names_{sex}.json     **参考队列**的型名（供对照）
#   MANIFEST.json                     全文件 sha256 + 口径参数（含 cluster 口径）
#
# **bundle 不随包分发**：受 UKB 材料转让协议约束，走受控下载。它里面没有任何
# 个体级数据（PLAN §1 第 1 条），但仍需下载方持有自己的 UKB application。

#' 载入 bundle
#'
#' @param path A local bundle directory. This is the only supported route; the
#'   bundle is distributed under controlled access.
#' @param verify Verify the sha256 of every file. Default `TRUE`.
#'   **Do not turn this off** -- it is where the "frozen representation"
#'   discipline lands on the R side.
#' @param quiet Suppress the summary.
#' @details
#' To obtain a bundle see `BUNDLE-ACCESS.md` in the package sources, or
#' <https://github.com/jiezhang-cn/ukbcareer/blob/main/BUNDLE-ACCESS.md>.
#' **There is no automatic download**: the weights derive from UK Biobank data,
#' and distribution requires the recipient to hold their own application --
#' a step code cannot take on your behalf.
#'
#' **The path must be pure ASCII.** libtorch resolves paths through the system
#' ANSI encoding, so non-ASCII characters (a non-Latin user name, for instance)
#' make `jit_load()` report "Parent directory does not exist". This function
#' copies the encoder to an ASCII cache directory and says so; choose the
#' location with `options(ukbcareer.ascii_cache = "...")`.
#' @return A `ukbcareer_bundle` object.
#' @examples
#' \dontrun{
#' b <- ukb_bundle("~/ukbcareer_bundle_v5.0")
#' b
#' }
#' @export
ukb_bundle <- function(path, verify = TRUE, quiet = FALSE) {
  if (missing(path) || is.null(path)) {
    stop("Point `path=` at a local bundle directory.\n",
         "  The bundle is neither shipped with the package nor downloaded ",
         "automatically: it is a derivative of UK Biobank data. ",
         "See the package home page.")
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  man_file <- file.path(path, "MANIFEST.json")
  if (!file.exists(man_file)) {
    stop("MANIFEST.json missing -- this is not a valid bundle directory: ", path)
  }
  man <- jsonlite::fromJSON(man_file, simplifyVector = FALSE)
  if (verify) .verify_manifest(path, man, quiet)

  b <- structure(
    list(path = path, manifest = man,
         version = man$bundle_version %||% "unknown",
         cluster = man$cluster %||% CLUSTER_DEFAULTS,
         tiers = .detect_tiers(path, man)),
    class = c("ukbcareer_bundle", "list")
  )
  ## 口径比对：bundle 的权威值 vs 本包的 DEFAULTS
  .check_settings(man)
  ## 读入随手就要用的小文件
  b$vocab <- .read_json(file.path(path, "vocab.json"))
  b$reference_qc <- .read_json(file.path(path, "reference_qc.json"))
  b$reliability <- .read_json(file.path(path, "reliability.json"))
  b$type_names <- stats::setNames(lapply(SEXES, function(s) {
    .read_json(file.path(path, sprintf("type_names_%s.json", s)))
  }), SEXES)
  if (!quiet) print(b)
  b
}


#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a


#' 逐文件校验 sha256
#'
#' **任何不符即 `stop()`** —— 与 Python 侧 `load_latent(enforce = TRUE)` 的
#' exit 1 对齐。表征与权重必须是同一次训练的产物，否则下游一切都不可信。
#' @noRd
.verify_manifest <- function(path, man, quiet = FALSE) {
  if (!requireNamespace("openssl", quietly = TRUE) &&
      !requireNamespace("digest", quietly = TRUE)) {
    warning("Neither openssl nor digest is available -> **skipping sha256 verification**. ",
            "Install one: install.packages(\"digest\")", call. = FALSE)
    return(invisible(NULL))
  }
  # **不要用 openssl::sha256(file(f, "rb"))** —— 那个连接不会被关掉，循环几十个
  # 文件后连接泄漏会污染后续读取，结果是几乎每个文件都"校验失败"（一个看起来
  # 很像"bundle 被改动过"的假警报）。digest 的 file= 是流式的，没有这个问题。
  sha <- function(f) {
    if (requireNamespace("digest", quietly = TRUE)) {
      digest::digest(f, algo = "sha256", file = TRUE)
    } else {
      as.character(openssl::sha256(readBin(f, "raw", file.size(f))))
    }
  }
  bad <- character(0)
  miss <- character(0)
  for (nm in names(man$files)) {
    f <- file.path(path, nm)
    if (!file.exists(f)) {
      miss <- c(miss, nm)
      next
    }
    if (!identical(sha(f), man$files[[nm]]$sha256)) bad <- c(bad, nm)
  }
  if (length(bad)) {
    stop("Files whose sha256 does not match: ", paste(bad, collapse = ", "),
         "\n  The bundle has been modified or downloaded incompletely. ",
         "**Refusing to continue**: the frozen representation is the only ",
         "guarantee of cross-study comparability.")
  }
  if (length(miss)) {
    warning(length(miss), " files are missing (",
            paste(utils::head(miss, 3), collapse = ", "),
            if (length(miss) > 3) " ..." else "",
            ") -> the corresponding features are unavailable", call. = FALSE)
  }
  if (!quiet) {
    message(sprintf("  sha256 verified: %d files",
                    length(man$files) - length(miss)))
  }
  invisible(NULL)
}


#' 哪几层可用
#' @noRd
.detect_tiers <- function(path, man) {
  has <- function(pat) length(Sys.glob(file.path(path, pat))) > 0L
  c(tier0 = file.exists(file.path(path, "vocab.json")),
    tier1 = has("soc_embed_*.parquet"),
    tier2 = has("encoder_*.ts"))
}


#' 口径比对
#'
#' bundle 的 MANIFEST 是权威值。不一致时 **warn 而不静默** ——
#' 静默的口径漂移是这个包最危险的失败模式。
#' @noRd
.check_settings <- function(man) {
  pairs <- list(
    c("max_len", "max_len"), c("age_min", "age_min"), c("age_max", "age_max"),
    c("d_model", "d_model"), c("soc_level", "level")
  )
  for (p in pairs) {
    got <- man[[p[1L]]]
    mine <- DEFAULTS[[p[2L]]]
    if (!is.null(got) && !is.null(mine) && !identical(as.character(got),
                                                      as.character(mine))) {
      warning(sprintf("Definition mismatch: the bundle has %s = %s, this package defaults to %s.",
                      p[1L], got, mine),
              "\nThe bundle wins, but the package and bundle versions ",
              "and results may not be comparable with the paper.", call. = FALSE)
    }
  }
  invisible(NULL)
}


#' @noRd
.read_json <- function(f) {
  if (!file.exists(f)) return(NULL)
  jsonlite::fromJSON(f, simplifyVector = FALSE)
}


#' 读 bundle 里的 353 × 192 职业码嵌入
#' @noRd
.bundle_embed <- function(bundle, sex) {
  f <- file.path(bundle$path, sprintf("soc_embed_%s.parquet", sex))
  if (!file.exists(f)) {
    stop("Missing ", basename(f), " -- Tier 1 (mobility geometry) unavailable")
  }
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("arrow is required to read parquet: install.packages(\"arrow\")")
  }
  data.table::as.data.table(arrow::read_parquet(f))
}


#' TorchScript 编码器的可用路径
#'
#' 非 ASCII 路径下 libtorch 加载会失败（见 [ukb_bundle] 的 details），
#' 这里把文件复制到一个 ASCII 缓存目录。**Windows 的 8.3 短路径名救不了** ——
#' 中文用户目录名本身没有短名（实测 `shortPathName()` 只缩写了后面几段）。
#' @noRd
.encoder_path <- function(bundle, sex) {
  f <- file.path(bundle$path, sprintf("encoder_%s.ts", sex))
  if (!file.exists(f)) stop("Missing ", basename(f), " -- Tier 2 unavailable")
  if (.is_ascii_path(f)) return(f)
  cache <- getOption("ukbcareer.ascii_cache",
                     file.path(Sys.getenv("SystemDrive", "C:"),
                               "ukbcareer-cache"))
  if (!.is_ascii_path(cache)) {
    stop("The bundle path contains non-ASCII characters, and so does the ",
         "fallback cache ", cache, ".\n",
         "  libtorch cannot load such a path. Move the bundle to an ASCII-only path, ",
         "or set options(ukbcareer.ascii_cache = \"C:/somewhere_ascii\")")
  }
  dir.create(cache, showWarnings = FALSE, recursive = TRUE)
  dst <- file.path(cache, basename(f))
  if (!file.exists(dst) || file.mtime(dst) < file.mtime(f)) {
    message("  Bundle path has non-ASCII characters -> copying the encoder to ", dst,
            "\n(libtorch resolves paths via the system ANSI encoding; ",
            "non-ASCII fails to load)")
    file.copy(f, dst, overwrite = TRUE)
  }
  dst
}


#' @export
print.ukbcareer_bundle <- function(x, ...) {
  m <- x$manifest
  cat("<ukbcareer_bundle>", x$version, "\n")
  cat("  path: ", x$path, "\n", sep = "")
  cat(sprintf("  settings: soc_level %s, max_len %s, ages [%s, %s], d_model %s\n",
              m$soc_level, m$max_len, m$age_min, m$age_max, m$d_model))
  cat(sprintf("  tiers available: %s\n",
              paste(names(x$tiers)[unlist(x$tiers)], collapse = " + ")))
  if (!is.null(m$cluster)) {
    cc <- m$cluster
    cat(sprintf(paste0("  clustering: Leiden resolution %s on an unweighted ",
                       "%s-NN graph of the raw %s-d z,\n              ",
                       "cluster floor max(%s*n, %s)\n"),
                cc$resolution, cc$knn_k, m$d_model, cc$min_cluster_frac,
                cc$min_cluster_n))
  }
  if (!is.null(x$reliability$typology)) {
    cat("  Stability of the reference partition",
        "(S1 bootstrap ARI; pre-registered line 0.60):\n")
    for (s in names(x$reliability$typology)) {
      t <- x$reliability$typology[[s]]
      flag <- if (isFALSE(t$passes_prespecified)) "  <- below the line" else ""
      cat(sprintf("    %-7s K=%-3s ARI %.3f +/- %.3f   largest cluster %.1f%%%s\n",
                  s, t$K, t$S1_bootstrap_ari, t$S1_bootstrap_sd,
                  100 * t$largest_cluster_frac, flag))
    }
    cat("  Note: re-running on your sample yields a DIFFERENT partition",
        "(K may differ;\n        type numbers and meanings do not carry over).",
        "What is comparable is the\n        method and the representation, not",
        "the partition. See ?ukb_typology\n")
  }
  invisible(x)
}


#' Reliability details of the bundle
#'
#' Prints S1 / ASW / largest cluster / SOC purity, and the cohort-level
#' perplexity baseline ladder. **Read this before putting `type` into a
#' regression.**
#' @param bundle The result of [ukb_bundle].
#' @export
ukb_bundle_info <- function(bundle) {
  stopifnot(inherits(bundle, "ukbcareer_bundle"))
  r <- bundle$reliability
  cat("== Partition diagnostics for the reference cohort ==\n")
  for (s in names(r$typology)) {
    t <- r$typology[[s]]
    cat(sprintf("\n%s (K = %s)\n", s, t$K))
    cat(sprintf("  S1 bootstrap ARI      %.3f +/- %.3f  (pre-registered line %.2f -> %s)\n",
                t$S1_bootstrap_ari, t$S1_bootstrap_sd, t$prespecified_min_ari,
                if (isTRUE(t$passes_prespecified)) "passes" else "**BELOW**"))
    cat(sprintf("  S3 birth-cohort ARI   %.3f\n", t$S3_birth_cohort_ari %||% NA))
    cat(sprintf("  ASW                   %.3f  (**not comparable to external\n",
                t$asw))
    cat("                               benchmarks**: distance concentration in\n")
    cat("                               high dimensions depresses it, and ASW is\n")
    cat("                               undefined across metric spaces)\n")
    cat(sprintf("  largest cluster       %.1f%%\n", 100 * t$largest_cluster_frac))
    cat(sprintf("  max SOC purity        %.3f%s\n", t$max_cluster_soc_purity,
                if (t$max_cluster_soc_purity > 0.9) {
                  "  <- at least one type is essentially a single occupation code"
                } else ""))
    if (!isTRUE(t$null_ari_valid)) {
      cat("  [note] The same-distribution null baseline is currently unusable: ",
          t$null_ari_caveat, "\n", sep = "")
    }
  }
  if (length(r$baselines)) {
    cat("\n== Cohort-level predictability baseline ladder (nats) ==\n")
    cat("  The encoder is **bidirectional** (no causal mask), so this measures\n")
    cat("  interpolation, not forecasting. The right comparator is",
        "'copy both neighbours':\n")
    for (s in names(r$baselines)) {
      b <- r$baselines[[s]]
      cat(sprintf("\n  %s\n", s))
      for (k in c("marginal", "persistence", "interpolation", "model_ce")) {
        if (!is.null(b[[k]])) {
          cat(sprintf("    %-14s %.3f  (ppl %.3f)\n", k, b[[k]], exp(b[[k]])))
        }
      }
    }
    cat("\n  -> The model gains only about 0.03 nats over 'copy both",
        "neighbours' (0.7-0.8%).\n")
    cat("     This is the first reason per-person perplexity is not returned;\n")
    cat("     the second is membership inference.\n")
  }
  invisible(bundle)
}
