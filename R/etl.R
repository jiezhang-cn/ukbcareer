# ══════════════════════════════════════════════════════════════════
# Tier 0：UKB Category 130 → 工作史事件表与年度面板
# ══════════════════════════════════════════════════════════════════
# `codes/py/01_worklife.py` 的 R 实现。**这是全部口径的所在地。**
# 每个函数对应 Python 侧的同名函数，逐列对齐（PLAN §9 的 Tier 0 黄金测试：
# 整数列完全相等，浮点列 max|Δ| < 1e-6）。

#' Job event table
#'
#' Slot index is job index (the sparse-array structure; see [.slot_cols]).
#' @noRd
.build_jobs <- function(d, entry, verbose = TRUE) {
  flds <- c(
    list(start = FIELDS$job_start, end = FIELDS$job_end, soc4 = FIELDS$soc4,
         oscar8 = FIELDS$oscar8, hours_cat = FIELDS$hours_cat,
         hours_exact = FIELDS$hours_exact, shift_any = FIELDS$shift_any,
         shift_day = FIELDS$shift_day, shift_mixed = FIELDS$shift_mixed,
         shift_night = FIELDS$shift_night, breath = FIELDS$breath),
    stats::setNames(as.list(EXPOSURE_FIELDS), paste0("exp_", names(EXPOSURE_FIELDS)))
  )
  L <- .to_long(d, flds)
  if (!nrow(L)) stop("No job fields found -- check that columns look like `<Name>__0_<slot>`")
  L[, eid := d$eid[row_id]]

  n_ongoing <- sum(!is.na(L$end) & L$end == CODINGS$ongoing)
  fill <- entry[match(L$eid, names(entry))]
  L[, start := .clean_year(start)]
  L[, end := .clean_year(end, ongoing_fill = fill)]
  for (k in names(EXPOSURE_FIELDS)) {
    cc <- paste0("exp_", k)
    if (cc %in% names(L)) L[, (cc) := .map_exposure(get(cc))]
  }

  # 工时：22604(40 槽) 与 22605(35 槽) **索引集互斥且并集 == range(22599)**
  # （实测 1.00000）—— 是二选一，不是"部分人额外填精确值"。取二者并集。
  hc <- if ("hours_cat" %in% names(L)) .hours_from_cat(L$hours_cat) else NA_real_
  he <- if ("hours_exact" %in% names(L)) as.numeric(L$hours_exact) else NA_real_
  L[, hours := data.table::fifelse(!is.na(he), he, hc)]

  # 轮班三级有序：0 从不 / 1 轮班非夜班 / 2 含夜班。
  # 依据：22620（是否轮班）有 12 万人可进主模型，但细节字段只剩 1,083–26,622 人 ——
  # 这个三级变量既用上了信息，又不受细节字段样本量限制。
  nd <- CODINGS$shift_notdone
  night <- if ("shift_night" %in% names(L)) as.numeric(L$shift_night) else NA_real_
  mixed <- if ("shift_mixed" %in% names(L)) as.numeric(L$shift_mixed) else NA_real_
  anys <- if ("shift_any" %in% names(L)) as.numeric(L$shift_any) else NA_real_
  has_night <- (!is.na(night) & night != nd) | (!is.na(mixed) & mixed != nd)
  L[, shift_level := data.table::fifelse(
    has_night, 2, data.table::fifelse(!is.na(anys) & anys == 1, 1, 0))]
  L[is.na(anys), shift_level := NA_real_]

  jobs <- L[!is.na(start)]
  n0 <- nrow(jobs)
  jobs <- jobs[is.na(end) | end >= start]
  if (verbose) {
    message(sprintf("  job records %s -> dropped %s with end before start -> %s",
                    fmt_n(n0), fmt_n(n0 - nrow(jobs)), fmt_n(nrow(jobs))))
    message(sprintf("  ongoing(-313): %s records -> filled with the PER-PERSON entry year (median %.0f)",
                    fmt_n(n_ongoing), stats::median(fill, na.rm = TRUE)))
  }
  jobs[, row_id := NULL]
  jobs[]
}


#' Gap event table
#'
#' The gap side is affected more by the ongoing fill than the job side
#' (75,317 spells in the reference cohort, 96.6% of them retirement).
#' @noRd
.build_gaps <- function(d, entry, verbose = TRUE) {
  L <- .to_long(d, list(gap_code = FIELDS$gap_code, start = FIELDS$gap_start,
                        end = FIELDS$gap_end))
  if (!nrow(L) || !all(c("gap_code", "start") %in% names(L))) {
    if (verbose) {
      message("  [warn] gap fields not found (Gap_coding / Year_gap_started / ",
              "Year_gap_ended) -> the annual panel will be built from jobs only. ",
              "**This silently loses 84.6% of gap person-years** (retirement ",
              "gaps are exactly the years with no concurrent job)")
    }
    return(data.table::data.table(eid = numeric(0), slot = integer(0),
                                  gap_code = numeric(0), start = numeric(0),
                                  end = numeric(0)))
  }
  L[, eid := d$eid[row_id]]
  fill <- entry[match(L$eid, names(entry))]
  n_og <- sum(!is.na(L$end) & L$end == CODINGS$ongoing)
  L[, start := .clean_year(start)]
  L[, end := .clean_year(end, ongoing_fill = fill)]
  g <- L[!is.na(gap_code) & !is.na(start)]
  g <- g[!is.na(end) & end >= start]
  if (verbose) {
    nr <- sum(g$gap_code == CODINGS$gap_retirement)
    nh <- sum(g$gap_code == CODINGS$gap_health)
    message(sprintf("  gap spells %s (%s people); %s ongoing -> filled with the per-person entry year",
                    fmt_n(nrow(g)), fmt_n(data.table::uniqueN(g$eid)), fmt_n(n_og)))
    message(sprintf(paste0("  retirement (108): %s spells (%.1f%%) -- treated as an END EVENT, ",
                           "not a break; illness/disability (106): %s (%.1f%%)"),
                    fmt_n(nr), 100 * nr / max(nrow(g), 1),
                    fmt_n(nh), 100 * nh / max(nrow(g), 1)))
  }
  g[, row_id := NULL]
  g[]
}


#' Annual panel from job UNION gap
#'
#' **Both must build it together.** Building the job panel first and merging
#' gaps into it silently loses 84.6% of gap person-years: the retirement gaps
#' are precisely the years with no concurrent job, so a merge drops them all.
#' @noRd
.to_annual <- function(jobs, gaps, birth, age_min, age_max) {
  exp_cols <- paste0("exp_", names(EXPOSURE_FIELDS))
  keep_job <- c("soc4", "oscar8", "hours", "shift_level", "breath", exp_cols)

  expand <- function(src, kind) {
    if (is.null(src) || !nrow(src)) return(NULL)
    s <- data.table::copy(src)
    yb <- birth[match(s$eid, names(birth))]
    # a0 向上取整、a1 向下取整：只有**完整覆盖**的年龄才算该年在职/在 gap
    s[, `:=`(a0 = ceiling(start - yb), a1 = floor(end - yb))]
    s <- s[is.finite(a0) & is.finite(a1)]
    s[, `:=`(a0 = pmax(a0, age_min), a1 = pmin(a1, age_max))]
    s <- s[a1 >= a0]
    if (!nrow(s)) return(NULL)
    n <- as.integer(s$a1 - s$a0 + 1L)
    rep_i <- rep.int(seq_len(nrow(s)), n)
    out <- s[rep_i]
    out[, age := a0 + (seq_len(.N) - 1L), by = rep_i]
    out[, kind := kind]
    out[]
  }

  jr <- expand(jobs, "job")
  gr <- expand(gaps, "gap")
  if (is.null(jr) && is.null(gr)) {
    stop("No person-years after expansion -- check that birth years line up with job/gap start and end years")
  }

  # 主工作：并行时取**工时最长者**，缺工时的排最后
  if (!is.null(jr)) {
    jr[, `:=`(pri = data.table::fifelse(is.na(hours), -1, hours))]
    data.table::setorder(jr, eid, age, pri)
    n_year <- jr[, .(n_jobs_year = .N), by = .(eid, age)]
    have <- intersect(keep_job, names(jr))
    main <- jr[, utils::tail(.SD, 1L), by = .(eid, age), .SDcols = have]
    A <- merge(main, n_year, by = c("eid", "age"), all = TRUE)
  } else {
    A <- data.table::data.table(eid = numeric(0), age = integer(0),
                                n_jobs_year = integer(0))
  }

  # gap 侧：**独立的键**，用外连接而非左连接 —— 这正是 (a) 那条修正
  if (!is.null(gr)) {
    gm <- gr[, .(gap_code = gap_code[1L]), by = .(eid, age)]
    A <- merge(A, gm, by = c("eid", "age"), all = TRUE)
  } else {
    A[, gap_code := NA_real_]
  }
  A[is.na(n_jobs_year), n_jobs_year := 0L]
  A[, in_job := as.integer(n_jobs_year > 0L)]
  A[, in_gap := as.integer(!is.na(gap_code))]

  # 真并行 vs 衔接年：UKB 只记年份，衔接看起来像并行。
  # 判据 = 有一份工作**严格跨过**该年（start < 该年 < end）。
  # 参考队列实测：同年 ≥2 工作 4.6% 人年，真并行仅 1.18% / 衔接 3.41%（约 3:1）。
  if (!is.null(jr)) {
    yb_j <- birth[match(jr$eid, names(birth))]
    jr[, interior := (start < age + yb_j) & (age + yb_j < end)]
    hi <- jr[, .(has_int = any(interior, na.rm = TRUE)), by = .(eid, age)]
    A <- merge(A, hi, by = c("eid", "age"), all.x = TRUE)
    A[is.na(has_int), has_int := FALSE]
  } else {
    A[, has_int := FALSE]
  }
  A[, is_parallel := as.integer(n_jobs_year > 1L & has_int)]
  A[, is_transition := as.integer(n_jobs_year > 1L & !has_int)]
  A[, has_int := NULL]
  data.table::setorder(A, eid, age)
  A[]
}


#' End of each person's working life
#'
#' `career_end = min(definite retirement age, last employed year + 1, cap)`,
#' where `cap` is the age at the questionnaire year.
#'
#' **The third term must be the per-person questionnaire year, not the index
#' date.** With the questionnaire year the observed-retirement rate goes from
#' 27.7% to 56.8% and the dominant source flips from "66% administrative
#' censoring" to "52% retirement" -- which is what makes `career_end` a
#' substantive event rather than a window edge. Every argument for
#' right-truncating at it holds only under that premise. It also drops
#' `r(seq_len, age_recruit)` from 0.695 to 0.408.
#'
#' Three boundary rules, each answering a case observed in the data:
#' 1. A "definite" retirement must have **no employed year after it**. 3.3% of
#'    people have employed years after their first retirement record
#'    (re-employment or part-time work); truncating at the first retirement
#'    year would cut away their real later history.
#' 2. 96.3% of `gap_code == 108` records are ongoing and so end at the
#'    questionnaire year: "retired at 55, answered in 2015" is stored as a
#'    gap more than ten years long. After truncation only its first year
#'    remains.
#' 3. People with no retirement record are truncated at **last employed year
#'    + 1** (5.6% last worked more than 5 years before the index date). They
#'    may be unrecorded retirements, long-term unemployment, or incomplete
#'    histories -- the three cannot be told apart, so one trailing year is
#'    kept to leave "the sequence stops here" visible, and
#'    `flag_incomplete` marks them.
#'
#' `career_end_source` must be carried downstream: it separates **retirees**
#' (for whom `career_end` is a substantive event) from the
#' **administratively censored** (for whom it is the edge of the observation
#' window). The variable means different things in the two groups; merging
#' them equates "retired at 53" with "still working when recruited at 53".
#' @noRd
.career_end <- function(annual, cap, age_min) {
  eids <- names(cap)
  a <- annual[, .(eid, age, in_job, gap_code)]
  a[, cap_i := cap[match(eid, eids)]]
  a <- a[age <= cap_i]
  data.table::setorder(a, eid, age)
  a[, is_ret := as.integer(in_job == 0L & !is.na(gap_code) &
                             gap_code == CODINGS$gap_retirement)]

  # 细则 1：该年之后是否还有在职年。反向累加 → shift(-1) 取"此后"的在职年数
  a[, rev_job := rev(cumsum(rev(in_job))), by = eid]
  a[, after := data.table::shift(rev_job, type = "lead", fill = 0L), by = eid]
  ret <- a[is_ret == 1L & after == 0L, .(retire = min(age)), by = eid]
  lastj <- a[in_job == 1L, .(last_job_plus1 = max(age) + 1L), by = eid]

  ce <- data.table::data.table(eid = as.numeric(eids), cap = as.numeric(cap))
  ce <- merge(ce, ret, by = "eid", all.x = TRUE)
  ce <- merge(ce, lastj, by = "eid", all.x = TRUE)
  m <- as.matrix(ce[, .(retire, last_job_plus1, cap)])
  # 三项取 min，NA 跳过。全 NA（既无在职年也无退休记录）→ cap
  out <- suppressWarnings(apply(m, 1L, min, na.rm = TRUE))
  out[!is.finite(out)] <- ce$cap[!is.finite(out)]
  src <- c("retire", "last_job_plus1", "cap")[
    suppressWarnings(apply(m, 1L, function(r) {
      if (all(is.na(r))) return(3L)
      which.min(replace(r, is.na(r), Inf))
    }))]
  ce[, `:=`(career_end = pmax(out, age_min), career_end_source = src)]
  ce[]
}


#' Right-truncate the annual panel at each person's cut age
#'
#' **Tokenisation and the exposure summary share this function**, so both
#' windows are fixed by the same cut vector and cannot drift apart.
#' @noRd
.truncate_annual <- function(annual, cut) {
  c_i <- cut[match(annual$eid, names(cut))]
  annual[!is.na(c_i) & age <= c_i]
}


#' Exposure and work-intensity summaries
#'
#' **Accumulated over `in_job == 1` person-years only, after
#' right-truncation at `career_end`.** Exposure in retirement and gap years is
#' `NA` by construction; counting those as zeros in the denominator would
#' dilute mean intensity by time out of work.
#'
#' `mean_*` follows the definition `(d1 + 2*d2) / (d0 + d1 + d2)`, range
#' **\[0, 2\]**, where d0/d1/d2 are employed years at intensity 0/1/2 and the
#' denominator counts only years where that agent is non-missing.
#'
#' `peak_*` stays `NA` when the agent was always "do not know", while `yrs_*`
#' is filled with 0. **The inconsistency is deliberate**: cumulative measures
#' need to be additive, peaks need to distinguish "not exposed" from "do not
#' know". The token side carries a separate indicator channel for the latter.
#' @noRd
.exposure_summary <- function(annual_trunc, long_hours_threshold) {
  w <- annual_trunc[in_job == 1L]
  if (!nrow(w)) return(data.table::data.table(eid = numeric(0)))
  o <- w[, .(work_years = .N), by = eid]
  for (n in names(EXPOSURE_FIELDS)) {
    cc <- paste0("exp_", n)
    if (!cc %in% names(w)) next
    s <- w[, {
      v <- get(cc)
      d1 <- sum(!is.na(v) & v == 1)
      d2 <- sum(!is.na(v) & v == 2)
      dn <- sum(!is.na(v))
      .(yrs = d1 + d2, yrs_high = d2,
        peak = if (dn == 0L) NA_real_ else max(v, na.rm = TRUE),
        mean = if (dn == 0L) NA_real_ else (d1 + 2 * d2) / dn)
    }, by = eid]
    data.table::setnames(s, c("yrs", "yrs_high", "peak", "mean"),
                         paste0(c("yrs_exp_", "yrs_high_", "peak_", "mean_"), n))
    o <- merge(o, s, by = "eid", all.x = TRUE)
  }
  ex <- w[, .(
    long_hours_years = sum(!is.na(hours) & hours >= long_hours_threshold),
    shift_years = sum(!is.na(shift_level) & shift_level > 0),
    night_shift_years = sum(!is.na(shift_level) & shift_level == 2)
  ), by = eid]
  o <- merge(o, ex, by = "eid", all.x = TRUE)
  # 累积量填 0（可加），峰值保留 NA（"没暴露"≠"不知道"）
  cum <- grep("^(yrs_|long_|shift_|night_|work_)", names(o), value = TRUE)
  for (cc in cum) o[is.na(get(cc)), (cc) := 0]
  o[]
}


# ══════════════════════════════════════════════════════════════════
#' UKB 工作史 ETL
#'
#' 把 UKB Category 130（+ Category 123 的 22500）的宽表变成五张干净的表：
#' job 事件表、gap 事件表、逐年面板、队列表（含 `career_end`）、暴露汇总。
#' 这是本包 Tier 0 的全部产出，**不需要 bundle、不需要模型权重**。
#'
#' @param path csv 路径，或一个已读入的 `data.frame` / `data.table`。
#' @param entry_path Category 123 的 csv 路径（含 `When_occupational_data_entered`）。
#'   **最容易漏的一个字段**：它在 Category 123 不在 130。缺它会回落到常数
#'   `fill_year`，代价是 ongoing 记录平均多算约 2 年 = 面板的 5.7%，
#'   且退休观察率从 56.8% 掉回 27.7% —— 那就不是论文的口径了。
#' @param covariate_path 协变量 csv 的路径（含 field 31 性别，可能也含 field 34
#'   出生年）。**真实的 Category 130 导出里通常没有性别列** —— 它是 field 31，
#'   属于基线数据，要单独提供。
#' @param sex 性别向量（与行对齐），或列名。缺省时依次在主表、`covariate_path`
#'   里找 `Sex__0_0` / `p31` / `sex`。
#' @param fields 覆盖默认列名映射的命名列表，如 `list(soc4 = "my_soc_col")`。
#' @param age_min,age_max 年龄窗口。`age_max = 75` 是**绑定约束**，
#'   参考队列 6.7% 的人被它封顶。
#' @param fill_year `entry_path` 缺失时 ongoing 的回落填充年。
#' @param verbose 打印各步人数与关键口径的实测占比。
#' @return `ukbcareer_worklife` 对象（一个列表）：`jobs` / `gaps` / `annual` /
#'   `cohort` / `exposure`，外加 `attr(x, "ukbcareer")` 里的口径元数据。
#' @examples
#' # 用包内的合成假人演示（真实用法把 ukb_synth() 换成你的 csv 路径）
#' wl <- ukb_worklife(ukb_synth(n = 40), verbose = FALSE)
#' names(wl)
#' @export
ukb_worklife <- function(path, entry_path = NULL, covariate_path = NULL,
                         sex = NULL, fields = NULL,
                         age_min = DEFAULTS$age_min, age_max = DEFAULTS$age_max,
                         fill_year = DEFAULTS$fill_year, verbose = TRUE) {
  if (!is.null(fields)) {
    bad <- setdiff(names(fields), names(FIELDS))
    if (length(bad)) stop("Unknown field name(s): ", paste(bad, collapse = ", "))
    old <- FIELDS
    FIELDS <- utils::modifyList(FIELDS, fields)
    on.exit(FIELDS <- old, add = TRUE)
  }
  d <- .read_wide(path)
  if (!"eid" %in% names(d)) stop("Input has no `eid` column")
  if (anyDuplicated(d$eid)) stop("`eid` has duplicates -- expected one row per person")
  if (verbose) message(sprintf("  read %s people x %d columns", fmt_n(nrow(d)), ncol(d)))

  ## 协变量表：真实导出里性别（field 31）与出生年（field 34）常在这里
  cov <- if (is.null(covariate_path)) NULL else .read_wide(covariate_path)
  if (!is.null(cov) && !"eid" %in% names(cov)) {
    stop("The table given by `covariate_path` has no `eid` column")
  }

  ## 出生年：时间轴对齐的锚点，没有它什么都做不了
  birth <- .lookup_col(d, cov, FIELDS$year_of_birth,
                       # 认 UKB 原始名与常见的清洗后名（各 dispensal 的清洗惯例不同）
                       c("Year_of_birth", "year_of_birth", "year_birth",
                         "yob", "p34", "f.34.0.0"),
                       "Year of birth (field 34)")
  birth <- stats::setNames(as.numeric(birth), as.character(d$eid))
  if (anyNA(birth)) stop(sum(is.na(birth)), " people have no year of birth -- cannot place them on the age axis")

  ## 逐人问卷录入年
  entry <- .entry_year(entry_path, d, fill_year, verbose)

  jobs <- .build_jobs(d, entry, verbose)
  ## **起始年 < 出生年 + min_work_age 的 job 整条丢弃。** 这不是可选的清洗 ——
  ## 黄金数据集里有人报 5 岁开始工作（1949 年起、1944 年生），保留它会让该人的
  ## 面板从 16 岁一路铺到退休，凭空多出三十几个在职人年。
  ## 注意判据是 job 的**起始年**而不是截断后的年龄：后者被 age_min 兜住，
  ## 于是脏记录会伪装成"16 岁就开始工作"而不被发现。
  n_young <- sum(jobs$start < birth[as.character(jobs$eid)] + DEFAULTS$min_work_age,
                 na.rm = TRUE)
  jobs <- jobs[is.na(start) |
                 start >= birth[as.character(eid)] + DEFAULTS$min_work_age]
  if (verbose && n_young) {
    message(sprintf("  start year < birth year + %d: dropped %s records",
                    DEFAULTS$min_work_age, fmt_n(n_young)))
  }
  gaps <- .build_gaps(d, entry, verbose)
  annual <- .to_annual(jobs, gaps, birth, age_min, age_max)

  ## cap = min(录入年 − 出生年, age_max)
  cap <- pmin(entry[names(birth)] - birth, age_max)
  names(cap) <- names(birth)
  ce <- .career_end(annual, cap, age_min)

  cut <- stats::setNames(ce$career_end, as.character(ce$eid))
  at <- .truncate_annual(annual, cut)
  expo <- .exposure_summary(at, DEFAULTS$long_hours_threshold)

  ## 队列表
  sexv <- .resolve_sex(d, cov, sex)
  n_obs <- at[, .(n_obs_years = .N, seq_len = max(age) - min(age) + 1L,
                  first_year = min(age), last_year = max(age)), by = eid]
  coh <- data.table::data.table(eid = d$eid, sex = sexv,
                                year_birth = as.numeric(birth[as.character(d$eid)]),
                                entry_year = as.numeric(entry[as.character(d$eid)]))
  coh <- merge(coh, ce[, .(eid, career_end, career_end_source,
                           retire_age = retire)], by = "eid", all.x = TRUE)
  coh <- merge(coh, n_obs, by = "eid", all.x = TRUE)
  ## **`observed_retirement` 是"有确定性退休记录"，不是"退休决定了 career_end"。**
  ## 两者不同：一个人可能有退休记录，但 `last_job_plus1` 或 cap 更小，于是
  ## `career_end_source != "retire"` 而他仍然是被观察到退休的。参考队列上
  ## 前者 56.8% 而后者约 52% —— 差的就是这批人。
  ## 这个量是 V5 验收门槛，也是用户会当协变量用的，口径不能错。
  coh[, observed_retirement := as.integer(!is.na(retire_age))]
  # **coverage 的分母必须是 career_end 而不是 age_recruit** —— 否则比值系统性 ≥1、
  # flag_incomplete 恒为 0，该规则完全空转。
  coh[, coverage := n_obs_years / pmax(career_end - age_min + 1L, 1L)]
  coh[, flag_incomplete := as.integer(!is.na(coverage) & coverage < 0.5)]
  coh[, max_parallel := 0L]
  mp <- at[, .(max_parallel = max(n_jobs_year)), by = eid]
  coh[mp, max_parallel := i.max_parallel, on = "eid"]

  if (verbose) {
    tb <- coh[!is.na(career_end_source), .N, by = career_end_source]
    tb[, pct := 100 * N / sum(N)]
    message("  career_end source: ",
            paste(sprintf("%s %.1f%%", tb$career_end_source, tb$pct),
                  collapse = " / "))
    message(sprintf("  observed retirement %.1f%%; %s person-years after truncation (median seq_len %.0f)",
                    100 * mean(coh$observed_retirement, na.rm = TRUE),
                    fmt_n(nrow(at)), stats::median(coh$seq_len, na.rm = TRUE)))
  }

  structure(
    list(jobs = jobs, gaps = gaps, annual = at, annual_untruncated = annual,
         cohort = coh, exposure = expo),
    class = c("ukbcareer_worklife", "list"),
    ukbcareer = list(
      version = .pkg_version(),
      age_min = age_min, age_max = age_max, truncate_at = "career_end",
      career_end_cap = "questionnaire",
      entry_year_source = attr(entry, "source"),
      n_people = nrow(coh), n_person_years = nrow(at)
    )
  )
}


#' @export
print.ukbcareer_worklife <- function(x, ...) {
  m <- attr(x, "ukbcareer")
  cat("<ukbcareer_worklife>\n")
  cat(sprintf("  %s people, %s person-years (right-truncated at career_end)\n",
              fmt_n(m$n_people), fmt_n(m$n_person_years)))
  cat(sprintf("  %s job records, %s gap spells\n", fmt_n(nrow(x$jobs)),
              fmt_n(nrow(x$gaps))))
  cat(sprintf("  age window [%d, %d]; entry-year source: %s\n",
              m$age_min, m$age_max, m$entry_year_source))
  s <- x$cohort[!is.na(career_end_source), .N, by = career_end_source]
  cat("  career_end source: ",
      paste(sprintf("%s %.1f%%", s$career_end_source, 100 * s$N / sum(s$N)),
            collapse = " / "), "\n", sep = "")
  invisible(x)
}
