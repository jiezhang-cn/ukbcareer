# ══════════════════════════════════════════════════════════════════
# UKB 宽表的列名解析与编码转换
# ══════════════════════════════════════════════════════════════════
# 对应 codes/py/common.py 的 parse_col / slot_cols / to_long。
# 列名格式 `<Name>__<instance>_<slot>`，如 `Year_job_started__0_3`。

## 字段名映射：本包内部名 → dispensal 的列名前缀（UKB field id 在注释里）。
## 用户的列名若不同，`ukb_worklife(fields = list(...))` 可逐项覆盖。
FIELDS <- list(
  year_of_birth = "Year_of_birth",              # 34
  n_jobs        = "Number_of_jobs_held",        # 22599
  n_gaps        = "Number_of_gap_periods",      # 22661
  job_start     = "Year_job_started",           # 22602
  job_end       = "Year_job_ended",             # 22603（-313 = ongoing）
  soc4          = "Job_code_historical",        # 22617，**4 位 SOC2000，353 值**
  oscar8        = "Job_coding",                 # 22601，8 位 OSCAR（前 4 位 == soc4）
  hours_cat     = "Work_hours_lumped_category", # 22604，40 槽
  hours_exact   = "Work_hours_per_week_exact_value",  # 22605，35 槽
  shift_any     = "Job_involved_shift_work",    # 22620，40 槽
  shift_day     = "Day_shifts_worked",          # 22630，33 槽
  shift_mixed   = "Mixture_of_day_and_night_shifts_worked",  # 22640
  shift_night   = "Night_shifts_worked",        # 22650
  gap_code      = "Gap_coding",                 # 22660，33 槽
  gap_start     = "Year_gap_started",
  gap_end       = "Year_gap_ended",
  breath        = "Breathing_problems_during_period_of_job",  # 22616
  entry_date    = "When_occupational_data_entered",  # 22500，**在 Category 123**
  sex           = "Sex"                          # 31
)

## 10 个暴露字段（22606–22615），顺序必须与 `EXPOSURES` 一致。
EXPOSURE_FIELDS <- c(
  noisy      = "Workplace_very_noisy",
  cold       = "Workplace_very_cold",
  hot        = "Workplace_very_hot",
  dusty      = "Workplace_very_dusty",
  fumes      = "Workplace_full_of_chemical_or_other_fumes",
  cigarette  = "Workplace_had_a_lot_of_cigarette_smoke_from_other_people_smoking",
  asbestos   = "Worked_with_materials_containing_asbestos",
  paints     = "Worked_with_paints_thinners_or_glues",
  pesticides = "Worked_with_pesticides",
  diesel     = "Workplace_had_a_lot_of_diesel_exhaust"
)


#' Parse a UKB column name
#'
#' @param col Character vector of column names.
#' @return A three-column data.table (`name` / `inst` / `slot`); rows that do
#'   not match the `<Name>__<inst>_<slot>` pattern are all `NA`.
#' @noRd
.parse_col <- function(col) {
  m <- regmatches(col, regexec("^(.+?)__([0-9]+)_([0-9]+)$", col))
  ok <- lengths(m) == 4L
  out <- data.table::data.table(
    col = col, name = NA_character_, inst = NA_integer_, slot = NA_integer_
  )
  if (any(ok)) {
    hit <- do.call(rbind, m[ok])
    out[ok, `:=`(name = hit[, 2L], inst = as.integer(hit[, 3L]),
                 slot = as.integer(hit[, 4L]))]
  }
  out
}


#' All slot columns of one field, in ascending slot order
#'
#' Short arrays in UK Biobank are **sparse arrays that preserve the job
#' index**: slot *k* is job *k*, not the *k*-th non-missing entry. Do not
#' re-map them.
#'
#' @param all_cols All column names of the table.
#' @param field Field-name prefix.
#' @param inst Instance, default 0.
#' @return Character vector of column names in slot order; length 0 if absent.
#' @noRd
# **已实测**：按 job 索引解释的一致率 1.00000，按"压缩到 0..m−1"解释只有
# 0.84285（118,527 人）。33 槽数组内部有空洞（33,516 人中 18,626 人有），
# 那是正常的 —— 空洞正是"该 job 未触发此字段"。
.slot_cols <- function(all_cols, field, inst = 0L) {
  p <- .parse_col(all_cols)
  # 用 base R 子集：`inst` 同时是列名与参数名，data.table 的 i 表达式里
  # `..inst` 走不通 fast-subset 路径（object '..inst' not found）
  keep <- !is.na(p$name) & p$name == field & p$inst == inst
  if (!any(keep)) return(character(0))
  sub <- p[keep, ]
  sub$col[order(sub$slot)]
}


#' Reshape several wide fields to long form, aligned on (row, slot)
#'
#' The only correct way to compare fields against each other: they have
#' different numbers of slots (15 / 14 / 20 / 23 / 33 / 35 / 40), and after an
#' outer join on slot the trailing slots of the shorter fields are `NA` --
#' which is exactly "this job did not trigger that field".
#'
#' @param d A data.table containing `eid`.
#' @param fields Named vector: output name -> field prefix.
#' @param inst Instance.
#' @return Long table with `row_id` / `slot` plus one numeric column per field.
#' @noRd
.to_long <- function(d, fields, inst = 0L) {
  all_cols <- names(d)
  parts <- list()
  for (nm in names(fields)) {
    sc <- .slot_cols(all_cols, fields[[nm]], inst)
    if (!length(sc)) next
    v <- as.matrix(d[, ..sc])
    storage.mode(v) <- "double"
    parts[[nm]] <- data.table::data.table(
      row_id = rep.int(seq_len(nrow(d)), length(sc)),
      slot = rep(seq_along(sc) - 1L, each = nrow(d)),
      value = as.vector(v)
    )
    data.table::setnames(parts[[nm]], "value", nm)
  }
  if (!length(parts)) return(data.table::data.table())
  out <- parts[[1L]]
  for (nm in names(parts)[-1L]) {
    out <- merge(out, parts[[nm]], by = c("row_id", "slot"), all = TRUE)
  }
  out[]
}


# ══════════════════════════════════════════════════════════════════
# UKB 编码转换
# ══════════════════════════════════════════════════════════════════

#' Workplace exposure codes -> ordinal 0/1/2; "do not know" -> NA
#'
#' **The negative codes are ordinal levels, not missing values**:
#' `0 < -131 < -141`. Cleaning on "negative means missing" destroys every
#' exposure variable. `-121` ("do not know") becomes `NA`, but it is *not*
#' absence of information -- a separate indicator channel records it.
#' @noRd
# 参考队列 16.5% 的在职人年至少一个 agent 答 DK，所以 token 侧必须另有一个
# DK 指示通道；不加它则 -121 被填成 0，与"从未暴露"不可区分。
# 构念效度已验证：dusty 各级对应"该工作期间有呼吸问题"的比例 2.8% / 5.9% / 12.5%。
.map_exposure <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep(NA_real_, length(x))
  out[!is.na(x) & x == 0] <- 0
  out[!is.na(x) & x == -131] <- 1
  out[!is.na(x) & x == -141] <- 2
  # -121 与其余未知码留 NA（已由 rep(NA) 初始化）
  out
}


#' Clean a year column: -313 (ongoing) -> the person's questionnaire year
#'
#' Without special handling this yields negative durations. `ongoing_fill`
#' **must be per person**: a constant adds about 2 years to every ongoing
#' record.
#'
#' @param x Numeric vector of years.
#' @param ongoing_fill Vector the same length as `x`, or a scalar.
#' @noRd
# 参考队列 job 44,049 条 / gap 75,317 段是 ongoing。用常数 2017 相对逐人真值
# 平均多算 1.98 年，合计约 236,667 人年 = 截断后面板的 5.7%，
# 受影响的量包括 work_years / yrs_exp_* / career_end。
.clean_year <- function(x, ongoing_fill = NULL) {
  x <- suppressWarnings(as.numeric(x))
  og <- CODINGS$ongoing
  if (is.null(ongoing_fill)) {
    x[!is.na(x) & x == og] <- NA_real_
  } else {
    f <- if (length(ongoing_fill) == 1L) rep(ongoing_fill, length(x)) else ongoing_fill
    i <- !is.na(x) & x == og
    x[i] <- f[i]
  }
  x[!is.na(x) & x < 1900] <- NA_real_
  x
}


#' Field 22604 hours band -> midpoint hours per week
#' @noRd
.hours_from_cat <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  m <- CODINGS$hours_midpoint
  out <- rep(NA_real_, length(x))
  for (k in names(m)) out[!is.na(x) & x == as.numeric(k)] <- m[[k]]
  out
}


#' 4-digit SOC code -> a coarser level
#'
#' Levels follow from integer division; **no external lookup table is
#' needed**, because the SOC2000 hierarchy is the digit structure itself.
#' @noRd
.soc_level <- function(soc4, level = "soc_l4") {
  if (!level %in% names(SOC_DIVISOR)) {
    stop("Unknown SOC level: ", level, ". Use one of ",
         paste(names(SOC_DIVISOR), collapse = " / "))
  }
  soc4 %/% SOC_DIVISOR[[level]]
}
