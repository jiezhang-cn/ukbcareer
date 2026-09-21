# ══════════════════════════════════════════════════════════════════
# 质控：把上游的项目纪律搬进 R 包
# ══════════════════════════════════════════════════════════════════
# 用户的样本不是我们的参考队列，**静默的口径漂移是本包最危险的失败模式**。
# 每一项检查都对应一个具体的失败原因，不是泛泛的"数据质量"。

#' Quality control: compare your definitions against the reference cohort
#'
#' Every check points at one specific cause:
#'
#' | Check | What exceeding it means |
#' | --- | --- |
#' | SOC out-of-vocabulary rate | your SOC coding version differs from the frozen vocabulary (expected near 0) |
#' | `career_end_source` shares | field 22500 is missing, or an index date was used as the cap |
#' | median `seq_len` | a systematic problem in the ETL |
#' | share hitting `max_len` | the reference cohort's maximum is exactly 60, so hitting 64 means the definitions differ |
#' | share hitting `age_max` | a different age definition |
#' | "do not know" rate | **the most common error**: negative exposure codes were cleaned away as missing |
#' | gap person-year share | the panel was not built from job UNION gap (which loses 84.6% of gap person-years) |
#' | parallel / transition shares | the two kinds of "two jobs in one year" were not separated |
#'
#' @param x The result of [ukb_worklife] or [ukb_career].
#' @param bundle The result of [ukb_bundle], which supplies the reference
#'   distributions. With `NULL` only internal consistency is checked.
#' @param quiet Return the table without printing.
#' @return A `data.table` with `check` / `value` / `value_num` / `reference` /
#'   `kind` / `status` / `note`. `status` is one of `ok` / `warn` / `fail` /
#'   `info`; `kind` is `cohort` (compared against the reference cohort, where
#'   a deviation may simply be a real sample difference) or `internal`
#'   (compared against a value that must hold, where a deviation is a bug).
#' @examples
#' wl <- ukb_worklife(ukb_synth(n = 60), verbose = FALSE)
#' ukb_qc(wl)
#' @export
ukb_qc <- function(x, bundle = NULL, quiet = FALSE) {
  wl <- if (inherits(x, "ukbcareer_result")) attr(x, "worklife") else x
  stopifnot(inherits(wl, "ukbcareer_worklife"))
  m <- attr(wl, "ukbcareer")
  ref <- if (!is.null(bundle)) bundle$reference_qc else NULL
  rows <- list()
  ## `yours` 分两列存：`value` 给人看（已格式化的字符串），`value_num` 给程序用。
  ## 合在一列里时 rbindlist 会因为混了字符串（如 entry_year_source）把整列
  ## 统一成 character，于是 is.numeric() 恒 FALSE、格式化全部失效 —— 症状是
  ## 打印出 0.0672746180305576 这样的原始浮点。
  ##
  ## `kind` 区分两种对照，否则 `reference` 一列混了两种语义，用户会误读。
  add <- function(check, yours, reference = NA_real_, status = "info",
                  note = "", kind = "cohort") {
    num <- suppressWarnings(as.numeric(yours))
    if (is.character(yours)) num <- NA_real_
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      check = check, value = .fmt_qc(yours), value_num = num,
      reference = as.numeric(reference), kind = kind,
      status = status, note = note)
  }
  ## 参考侧按人数加权合并两性
  rget <- function(f) {
    if (is.null(ref)) return(NA_real_)
    v <- vapply(ref$by_sex, function(q) {
      r <- f(q)
      if (is.null(r) || !is.numeric(r)) NA_real_ else as.numeric(r)
    }, numeric(1))
    w <- vapply(ref$by_sex, function(q) as.numeric(q$n), numeric(1))
    ok <- !is.na(v)
    if (!any(ok)) return(NA_real_)
    stats::weighted.mean(v[ok], w[ok])
  }
  cmp <- function(yours, reference, tol, kind = "abs") {
    if (is.na(reference) || is.na(yours)) return("info")
    d <- if (kind == "abs") abs(yours - reference) else
      abs(yours - reference) / max(abs(reference), 1e-9)
    if (d <= tol) "ok" else "warn"
  }

  co <- wl$cohort
  a <- wl$annual
  n <- nrow(co)

  ## ── 口径元数据 ────────────────────────────────────────────
  add("n_people", n, rget(function(q) q$n), "info")
  ey <- m$entry_year_source
  add("entry_year_source", ey, NA_real_,
      if (identical(ey, "field_22500")) "ok" else "fail",
      if (identical(ey, "field_22500")) "field 22500 was used" else
        paste0("**Fell back to a constant.** Ongoing records then gain about ",
               "2 years each (5.7% of all person-years) and the ",
               "observed-retirement rate drops from 56.8% to 27.7%. These are ",
               "no longer the published definitions. Field 22500 lives in ",
               "Category 123, not 130."))

  ## ── seq_len 与封顶 ────────────────────────────────────────
  sl <- stats::median(co$seq_len, na.rm = TRUE)
  rsl <- rget(function(q) q$seq_len$p50)
  add("median seq_len", sl, rsl, cmp(sl, rsl, 3),
      "A deviation beyond 3 years suggests a systematic ETL problem")
  ml <- as.integer(m$max_len %||% DEFAULTS$max_len)
  hit <- mean(co$seq_len >= ml - 1L, na.rm = TRUE)
  add("share hitting max_len", hit, rget(function(q) q$frac_hit_max_len),
      if (is.na(hit)) "info" else if (hit <= 0.01) "ok" else "warn",
      paste0("The reference cohort's maximum seq_len is exactly 60, so ",
             "hitting 64 means the definitions differ"))
  cap <- mean(co$career_end >= m$age_max, na.rm = TRUE)
  rcap <- rget(function(q) q$frac_capped_age_max)
  add("share hitting age_max", cap, rcap, cmp(cap, rcap, 0.05),
      "age_max = 75 is a binding constraint; 6.7% of the reference cohort hit it")

  ## ── career_end 来源 ───────────────────────────────────────
  for (s in c("retire", "last_job_plus1", "cap")) {
    y <- mean(co$career_end_source == s, na.rm = TRUE)
    r <- if (is.null(ref)) NA_real_ else rget(function(q) {
      v <- q$career_end_source[[s]]
      if (is.null(v)) 0 else v
    })
    add(paste0("career_end_source = ", s), y, r, cmp(y, r, 0.10),
        if (s == "retire") {
          paste0("About 52% in the reference cohort. If yours is far lower, ",
                 "check field 22500 and the gap fields")
        } else "")
  }
  or <- mean(co$observed_retirement, na.rm = TRUE)
  add("observed retirement", or, rget(function(q) q$observed_retirement_frac),
      cmp(or, rget(function(q) q$observed_retirement_frac), 0.10),
      paste0("56.8% in the reference cohort; using an index date as the cap ",
             "drops it to 27.7%"))

  ## ── 面板结构 ──────────────────────────────────────────────
  gp <- mean(a$in_gap == 1L)
  rgp <- rget(function(q) q$frac_person_years_in_gap)
  add("gap person-year share", gp, rgp, cmp(gp, rgp, 0.10),
      paste0("Far below the reference suggests the panel was not built from ",
             "job UNION gap, which loses 84.6% of gap person-years"))
  pl <- mean(a$is_parallel == 1L)
  tr <- mean(a$is_transition == 1L)
  add("truly parallel person-years", pl, rget(function(q) q$frac_parallel),
      cmp(pl, rget(function(q) q$frac_parallel), 0.02),
      "1.18% in the reference cohort")
  add("transition-year person-years", tr, rget(function(q) q$frac_transition),
      cmp(tr, rget(function(q) q$frac_transition), 0.03),
      paste0("3.41% in the reference cohort. UK Biobank records years only, ",
             "so a handover looks like parallel work; the two must be separated"))

  ## ── 暴露：DK 率是最灵敏的探针 ─────────────────────────────
  ec <- intersect(paste0("exp_", EXPOSURES), names(a))
  if (length(ec)) {
    inj <- a[in_job == 1L]
    dk <- mean(rowSums(is.na(inj[, ..ec])) > 0)
    rdk <- rget(function(q) q$dk_any_frac_in_job)
    add("\"do not know\" rate (employed years)", dk, rdk, cmp(dk, rdk, 0.08),
        paste0("About 16.5% in the reference cohort. **Far below usually means ",
               "the negative codes were cleaned away as missing** -- ",
               "0 < -131 < -141 are ordinal levels; only -121 is \"do not know\""))
    ## 暴露取值必须落在 {0,1,2}
    vals <- unique(unlist(lapply(ec, function(cc) unique(a[[cc]]))))
    vals <- vals[!is.na(vals)]
    bad <- setdiff(vals, c(0, 1, 2))
    add("exposure values out of range", length(bad), 0,
        if (!length(bad)) "ok" else "fail",
        if (length(bad)) {
          paste0("Found ", paste(utils::head(bad, 5), collapse = ","),
                 " -- only 0/1/2 are valid (NA means \"do not know\")")
        } else "",
        kind = "internal")
  }

  ## ── SOC 词表覆盖 ──────────────────────────────────────────
  if (!is.null(bundle)) {
    sv <- unlist(bundle$vocab$soc_vocab)
    lv <- bundle$vocab$level %||% DEFAULTS$level
    inj <- a[in_job == 1L & !is.na(soc4)]
    code <- .soc_level(as.numeric(inj$soc4), lv)
    unk <- mean(!as.character(code) %in% names(sv))
    add("SOC out-of-vocabulary rate", unk, rget(function(q) q$unk_soc_frac),
        if (unk <= 0.02) "ok" else "warn",
        paste0("0.0000% in the reference cohort (field 22617 takes exactly ",
               "353 values cohort-wide). Above 2% means a different coding version"))
    ## 口径参数比对
    for (p in c("age_min", "age_max")) {
      mv <- bundle$manifest[[p]]
      if (!is.null(mv)) {
        add(paste0("setting ", p), m[[p]], as.numeric(mv),
            if (identical(as.numeric(m[[p]]), as.numeric(mv))) "ok" else "fail",
            "If this differs from the bundle, results are not comparable with the paper",
            kind = "internal")
      }
    }
  }

  ## ── 内部一致性（不需要参考队列，偏离一定是 bug）──────────
  n_cov <- sum(co$coverage > 1, na.rm = TRUE)
  add("people with coverage > 1", n_cov, 0, if (n_cov == 0L) "ok" else "fail",
      paste0("The denominator of coverage must be career_end; using ",
             "age_recruit makes the ratio systematically >= 1 and the ",
             "flag_incomplete rule a no-op"),
      kind = "internal")
  add("flag_incomplete share", mean(co$flag_incomplete, na.rm = TRUE),
      rget(function(q) q$frac_flag_incomplete), "info",
      "People with a long blank stretch inside their observation window")
  no_py <- sum(!co$eid %in% a$eid)
  add("people with no person-years", no_py, NA_real_,
      if (no_py == 0L) "ok" else "info",
      paste0("People whose work history is entirely unusable (e.g. end before ",
             "start); downstream values will be NA"),
      kind = "internal")

  out <- data.table::rbindlist(rows, use.names = TRUE)
  if (!quiet) .print_qc(out, is.null(ref))
  invisible(out)
}


#' 数值格式化：比例给 4 位小数，计数给千位分隔，字符串原样
#' @noRd
.fmt_qc <- function(v) {
  if (is.character(v) || is.factor(v)) return(as.character(v))
  v <- as.numeric(v)
  if (is.na(v)) return("NA")
  if (v > 0 && v < 1) sprintf("%.4f", v) else format(v, big.mark = ",")
}


#' @noRd
.print_qc <- function(q, no_ref) {
  sym <- c(ok = "ok  ", warn = "WARN", fail = "FAIL", info = "--  ")
  cat("== ukbcareer quality control ==\n")
  if (no_ref) {
    cat("  (no bundle given -> internal consistency only, no reference cohort)\n")
  }
  cat("  kind: cohort   = against the reference cohort (a deviation may be a",
      "real sample difference)\n")
  cat("        internal = against a value that must hold (a deviation is a",
      "bug or a wrong definition)\n\n")
  for (i in seq_len(nrow(q))) {
    r <- q[i]
    rf <- if (is.na(r$reference)) "" else {
      sprintf("  (%s %s)", if (r$kind == "internal") "expected" else "reference",
              .fmt_qc(r$reference))
    }
    cat(sprintf("  [%s] %-34s %12s%s\n", sym[[r$status]], r$check,
                r$value, rf))
    if (r$status %in% c("warn", "fail") && nzchar(r$note)) {
      cat("         -> ", r$note, "\n", sep = "")
    }
  }
  nf <- sum(q$status == "fail")
  nw <- sum(q$status == "warn")
  cat(sprintf("\n  %d fail, %d warn. ", nf, nw))
  if (nf) {
    cat("**Every fail must be resolved** -- it means your definitions differ",
        "from the paper's and results are not comparable.\n")
  } else if (nw) {
    cat("A warn is not necessarily an error (it may be a real sample",
        "difference), but it must be explainable.\n")
  } else {
    cat("Definitions agree with the reference cohort.\n")
  }
  invisible(q)
}
