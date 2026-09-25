# ══════════════════════════════════════════════════════════════════
# Tier 0：9 状态逐年序列（接 TraMineR）
# ══════════════════════════════════════════════════════════════════
# 对应 codes/py/02_tokenize.py 的 derive_state / state_sequences。

#' 年度面板 → 9 状态
#'
#' 优先级：**在职 > gap**（同年兼有算在职）。在职年再按工时分全职/兼职。
#'
#' 为什么是 9 个状态而不是把小类合成一个 `other_break`：参考队列 361,806 个
#' 非在职人年的实测构成是 105 照顾家庭 57.5% / **101 每周<15h 或<6 个月的有薪工作
#' 15.4%** / 103 教育 9.7% / **108 退休 9.0%** / -717+102 其他 5.6% /
#' 107 失业 1.6% / **106 因病或残疾 1.0%** / -818+-121 0.2%。加粗的三个必须单列 ——
#' 101 是有薪工作却被归进中断（第二大非在职类别）；106 是最有解释价值的中断类型之一，
#' 也是健康选择偏倚的直接指标；108 的残余主要是 `career_end` 那一年本身。
#' 混成一个 `other_break`（占非在职人年 31.2%）读不出任何状态的含义。
#'
#' @param annual 年度面板（`ukb_worklife()` 的 `annual`）。
#' @param hours_ft_threshold 全职工时门槛，默认 30。
#' @return 整数向量，1..10（10 = unknown），与 `annual` 的行对齐。
#' @noRd
.derive_state <- function(annual, hours_ft_threshold = DEFAULTS$hours_ft_threshold) {
  ft <- hours_ft_threshold
  in_job <- annual$in_job == 1L
  hours <- if ("hours" %in% names(annual)) as.numeric(annual$hours) else NA_real_
  hours <- rep_len(hours, nrow(annual))
  # **hours 的缺失填充**：截断后面板里 hours 实测缺失率 0%，这个默认值从未生效。
  # 保留是防御性的，但若将来缺失出现它会把缺失静默算成全职 —— 所以要有断言钉住。
  h <- data.table::fifelse(is.na(hours), ft, hours)
  st <- data.table::fifelse(in_job, data.table::fifelse(h >= ft, 1L, 2L), 9L)
  gc <- if ("gap_code" %in% names(annual)) {
    as.numeric(annual$gap_code)
  } else {
    rep(NA_real_, nrow(annual))
  }
  for (k in names(GAP_TO_STATE)) {
    hit <- !in_job & !is.na(gc) & gc == as.numeric(k)
    st[hit] <- GAP_TO_STATE[[k]]
  }
  st
}


#' Annual state sequences (9 states + unknown) in wide format
#'
#' Classifies every person-year into one of 9 substantive labour-market states
#' (plus `unknown`) and lays them out as a person-by-age matrix. Columns run
#' from age `age_min` to each person's `career_end`; after that the cells are
#' `NA`.
#'
#' Rules: **being in a job takes priority over a gap** (a year with both counts
#' as in work), and work years are split into full-time and part-time by
#' weekly hours.
#'
#' **The trailing `NA`s carry information** ("career has ended"). Do not
#' impute them as missing data -- in a chronogram they are a layer of their
#' own.
#'
#' @section Why 9 states rather than one `other_break`:
#' In the reference cohort, the 361,806 person-years spent out of work break
#' down as: looking after home/family (code 105) 57.5%; **paid work under 15
#' h/week or under 6 months (101) 15.4%**; education (103) 9.7%;
#' **retirement (108) 9.0%**; other (-717 and 102) 5.6%; unemployment (107)
#' 1.6%; **sickness or disability (106) 1.0%**; -818 and -121 0.2%. The three
#' in bold must be kept separate: 101 is paid work that would otherwise be
#' filed as a break (and is the second-largest non-work category); 106 is one
#' of the most informative break types and a direct marker of health
#' selection; what remains of 108 is mostly the `career_end` year itself.
#' Lumping them into a single `other_break` (31.2% of non-work person-years)
#' would make the state uninterpretable.
#'
#' @param x Output of [ukb_career()] or [ukb_worklife()].
#' @param sex Keep only this sex; `NULL` (default) keeps everyone.
#' @param hours_ft_threshold Weekly hours at or above which a job year counts
#'   as full-time (default 30).
#' @return A `ukbcareer_states` object: an integer matrix (rows = people,
#'   columns = ages) with attributes `"alphabet"` (state labels), `"colours"`,
#'   `"eid"` and `"ages"`.
#' @examples
#' wl <- ukb_worklife(ukb_synth(n = 40), verbose = FALSE)
#' st <- ukb_states(wl)
#' dim(st)
#' @export
ukb_states <- function(x, sex = NULL,
                       hours_ft_threshold = DEFAULTS$hours_ft_threshold) {
  x <- .as_worklife(x)
  a <- data.table::copy(x$annual)
  if (!is.null(sex)) {
    keep <- x$cohort$eid[as.character(x$cohort$sex) %in% as.character(sex)]
    if (!length(keep)) stop("No people with sex ", paste(sex, collapse = "/"))
    a <- a[eid %in% keep]
  }
  if (!nrow(a)) stop("No person-years available")
  a[, state := .derive_state(a, hours_ft_threshold)]
  # 同一 (eid, age) 只留一行 —— 面板理论上已唯一，这里是防御
  a <- unique(a, by = c("eid", "age"))
  age_lo <- attr(x, "ukbcareer")$age_min
  age_hi <- max(a$age)
  ages <- age_lo:age_hi
  eids <- sort(unique(a$eid))
  m <- matrix(NA_integer_, nrow = length(eids), ncol = length(ages),
              dimnames = list(as.character(eids), as.character(ages)))
  m[cbind(match(a$eid, eids), match(a$age, ages))] <- a$state
  structure(m, class = c("ukbcareer_states", "matrix", "array"),
            alphabet = STATE_NAMES, colours = STATE_COLOURS,
            eid = eids, ages = ages)
}


#' @export
print.ukbcareer_states <- function(x, ...) {
  cat(sprintf("<ukbcareer_states> %s people x %d age positions (ages %s-%s)\n",
              fmt_n(nrow(x)), ncol(x), colnames(x)[1L],
              colnames(x)[ncol(x)]))
  tb <- table(factor(STATE_NAMES[as.vector(x)], levels = STATE_NAMES))
  tb <- tb[tb > 0]
  cat("  State composition (share of observed person-years):\n")
  for (i in seq_along(tb)) {
    cat(sprintf("    %-14s %6.2f%%\n", names(tb)[i], 100 * tb[[i]] / sum(tb)))
  }
  cat(sprintf("  trailing NA (career ended): %.1f%% -- information, not missingness\n",
              100 * mean(is.na(as.vector(x)))))
  invisible(x)
}


#' Convert state sequences to a TraMineR `stslist`
#'
#' Hands the output of [ukb_states] to TraMineR, with the package's state
#' labels and colour palette already set, so you can use the usual sequence
#' analysis tools (`seqdplot()`, `seqdist()`, ...). Requires the TraMineR
#' package.
#'
#' @param x Output of [ukb_states].
#' @param ... Further arguments passed to `TraMineR::seqdef()`.
#' @return A TraMineR `stslist` object.
#' @export
as_seqdef <- function(x, ...) {
  stopifnot(inherits(x, "ukbcareer_states"))
  if (!requireNamespace("TraMineR", quietly = TRUE)) {
    stop("TraMineR is required: install.packages(\"TraMineR\")")
  }
  alpha <- attr(x, "alphabet")
  d <- as.data.frame(x)
  for (j in seq_along(d)) d[[j]] <- alpha[d[[j]]]
  TraMineR::seqdef(d, alphabet = alpha, cpal = unname(attr(x, "colours")),
                   id = attr(x, "eid"), ...)
}
