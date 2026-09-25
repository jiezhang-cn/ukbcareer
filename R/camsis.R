# ══════════════════════════════════════════════════════════════════
# Tier 0：CAMSIS 职业地位（用户自备量表）
# ══════════════════════════════════════════════════════════════════
# 对应 codes/py/01_worklife.py 的 build_camsis / build_camsis_by_age。
#
# **CAMSIS 表不随本包分发**（用户 2026-09-21 拍定）：它是外部资源，许可另计。
# 用 `ukb_camsis(wl, path)` 自备。可从 CAMSIS 项目站点取 SOC2000 版。
#
# **承重限制，必须进方法学部分**：CAMSIS 官方未建原生 SOC2000 版本，现有分数由
# SOC90 经 Occupational Information Unit 索引近似映射（"Britain 2001
# approximations" v0.1）。在型画像里它是脚注；在桑基图里它是**纵轴** ——
# "职业流动绝大多数是横向的"这个结论直接依赖这个映射。

#' Attach CAMSIS occupational status scores
#'
#' Looks up a CAMSIS status score for every working person-year and
#' summarises each person's status trajectory (mean, first, last, change,
#' slope, and a rising / flat / declining pattern). Works from your CSVs
#' alone -- no model bundle needed. **The CAMSIS table is not shipped with the
#' package** (it is an external resource with its own licence); download the
#' SOC2000 version from the CAMSIS project site and pass its path here.
#'
#' Scores are computed only on **working** person-years up to `career_end`
#' (the same window as the model input).
#'
#' **The sex-specific column is used** (`fcamsis` for women, `mcamsis` for
#' men): sex specificity is built into the scale. As a consequence, **the two
#' sexes are not measured on the same ruler** -- you can compare how much
#' mobility is lateral, but not which occupation sits at a given score.
#'
#' CAMSIS is **not an input to the model**. It is a deterministic function of
#' the SOC code (same code, same score), so feeding it in would just give the
#' model the occupation code twice in another form. Its value lies precisely
#' in being **independent of the representation**: checking whether the
#' latent space recovers the occupational status gradient is only a meaningful
#' test because CAMSIS was left out.
#'
#' @section Limitation to report in your methods:
#' CAMSIS has no native SOC2000 version. The available scores are approximated
#' from SOC90 via Occupational Information Unit indices ("Britain 2001
#' approximations" v0.1). In type profiles this is a footnote; in the sankey
#' from [plot_flows] CAMSIS gives each node its status position and decides
#' whether a move counts as up, down or lateral, so the conclusion "most
#' occupational mobility is lateral" rests directly on this mapping.
#'
#' @param x Output of [ukb_worklife].
#' @param path Path to the CAMSIS CSV, or a data.frame. It needs three
#'   columns: `soc2000` (4-digit code), `mcamsis` and `fcamsis`. Column names
#'   are case-insensitive.
#' @param slope_flat Slope threshold (CAMSIS points per year) below which a
#'   trajectory counts as flat; default 0.05. Slopes are only fitted for people
#'   with at least 5 scored years spanning at least 3 years of age; otherwise
#'   they are `NA`.
#' @param verbose If `TRUE`, print coverage and the mix of trajectory patterns.
#' @return `x` with two added elements: `camsis` (one row per person) and
#'   `camsis_by_age` (long table with one row per working person-year, needed
#'   to plot trajectories by age, which the per-person summary cannot give).
#'   In `camsis`, `zero_slope = 1` flags people whose score never changed, for
#'   whom a slope of 0 is exact rather than fitting noise.
#' @examples
#' \dontrun{
#' wl <- ukb_worklife("ukb_cat130.csv", entry_path = "ukb_cat123.csv")
#' wl <- ukb_camsis(wl, "camsis_soc2000.csv")
#' }
#' @export
ukb_camsis <- function(x, path, slope_flat = 0.05, verbose = TRUE) {
  stopifnot(inherits(x, "ukbcareer_worklife"))
  cm <- .read_wide(path)
  data.table::setnames(cm, tolower(names(cm)))
  need <- c("soc2000", "mcamsis", "fcamsis")
  miss <- setdiff(need, names(cm))
  if (length(miss)) {
    stop("CAMSIS table is missing column(s): ", paste(miss, collapse = ", "),
         ". It needs soc2000 / mcamsis / fcamsis")
  }
  cm <- cm[, .(soc4 = as.numeric(soc2000), mcamsis = as.numeric(mcamsis),
               fcamsis = as.numeric(fcamsis))]
  cm <- unique(cm, by = "soc4")
  if (verbose) {
    message(sprintf(paste0("  CAMSIS table: %s rows / %s unique 4-digit SOC2000 codes -- ",
                           "353 is this table's NATIVE granularity, matching the model's soc_level"),
                    fmt_n(nrow(cm)), fmt_n(data.table::uniqueN(cm$soc4))))
  }

  a <- x$annual[in_job == 1L, .(eid, age, soc4)]
  a[x$cohort, sex := i.sex, on = "eid"]
  a <- merge(a, cm, by = "soc4", all.x = TRUE)
  a[, camsis := data.table::fifelse(as.character(sex) == "Female",
                                    fcamsis, mcamsis)]
  cover <- mean(!is.na(a$camsis))
  if (verbose) {
    message(sprintf("  CAMSIS covers %.4f of employed person-years%s", cover,
                    if (cover > 0.95) "" else "  [warn] low -- check your SOC coding version"))
  }
  a <- a[!is.na(camsis)]
  data.table::setorder(a, eid, age)

  # 逐人斜率：**点数不足或年龄跨度太小时给 NA**，不要硬拟合。
  # 上游的教训：`polyfit` 在**常数 y** 上会返回 3.2e-16 这样的浮点噪声，
  # 于是"逐人斜率的中位数"落在 0 这个原子上，把所有型都标成 flat。
  # 终生只报一个职业码的人（CAMSIS 从未变过）正是这种情形，占比不小。
  sl <- a[, {
    if (.N < 5L || diff(range(age)) < 3) {
      .(camsis_slope = NA_real_, zero_slope = NA_integer_)
    } else if (stats::var(camsis) == 0) {
      # 显式标出"终生单码" —— 它的斜率是真 0 而非拟合噪声
      .(camsis_slope = 0, zero_slope = 1L)
    } else {
      .(camsis_slope = unname(stats::coef(stats::lm(camsis ~ age))[2L]),
        zero_slope = 0L)
    }
  }, by = eid]
  agg <- a[, .(camsis_mean = mean(camsis),
               camsis_first = camsis[1L],
               camsis_last = camsis[.N],
               camsis_n_years = .N), by = eid]
  out <- merge(agg, sl, by = "eid", all = TRUE)
  out[, camsis_change := camsis_last - camsis_first]
  out[, camsis_pattern := data.table::fifelse(
    is.na(camsis_slope), NA_character_,
    data.table::fifelse(camsis_slope > slope_flat, "rising",
                        data.table::fifelse(camsis_slope < -slope_flat,
                                            "declining", "flat")))]
  if (verbose) {
    tb <- out[!is.na(camsis_pattern), .N, by = camsis_pattern]
    message("  status trajectory patterns: ",
            paste(sprintf("%s %s", tb$camsis_pattern, fmt_n(tb$N)),
                  collapse = " / "))
    nz <- sum(out$zero_slope == 1L, na.rm = TRUE)
    if (nz) {
      message(sprintf(paste0("  of which %s reported a single occupation code for life ",
                             "(CAMSIS never changed) -- their flat slope is a true ",
                             "zero, not fitting noise"), fmt_n(nz)))
    }
  }

  x$camsis <- out[]
  x$camsis_by_age <- a[, .(eid, age, soc4, camsis)]
  at <- attr(x, "ukbcareer")
  at$camsis_coverage <- cover
  at$camsis_caveat <- paste0(
    "CAMSIS has no native SOC2000 version; scores are approximated from SOC90 ",
    "via Occupational Information Unit indices (Britain 2001 approximations ",
    "v0.1). The scale is sex-specific, so the two sexes are not on the same ruler.")
  attr(x, "ukbcareer") <- at
  x
}
