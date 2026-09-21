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


#' 9 状态逐年序列（宽表）
#'
#' 横轴是年龄 `age_min` → 该人的 `career_end`，之后留 `NA`。
#' **右侧的 NA 本身就是信息**（"职业生涯已结束"），不要把它当缺失填掉 ——
#' 画 chronogram 时它是独立的一层。
#'
#' @param x [ukb_worklife] 的返回值。
#' @param sex 只要某个性别；`NULL` 表示全部。
#' @param hours_ft_threshold 全职工时门槛。
#' @return 一个 `ukbcareer_states` 对象：矩阵（行 = 人，列 = 年龄），
#'   加上 `attr(, "alphabet")` / `attr(, "colours")` / `attr(, "eid")`。
#' @examples
#' wl <- ukb_worklife(ukb_synth(n = 40), verbose = FALSE)
#' st <- ukb_states(wl)
#' dim(st)
#' @export
ukb_states <- function(x, sex = NULL,
                       hours_ft_threshold = DEFAULTS$hours_ft_threshold) {
  stopifnot(inherits(x, "ukbcareer_worklife"))
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


#' 转成 TraMineR 的 `stslist`
#'
#' @param x [ukb_states] 的返回值。
#' @param ... 传给 `TraMineR::seqdef()`。
#' @return 一个 `stslist`。
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
