# ══════════════════════════════════════════════════════════════════
# 合成假人 —— 测试、示例与绘图 smoke test 的唯一数据源
# ══════════════════════════════════════════════════════════════════
# **真实 UKB 数据不能进包**（PLAN §1）。黄金测试用的 2,000 人输入留在服务器，
# 包内只放这个生成器。它刻意覆盖 ETL 的每一条边界分支 —— 见 `branches` 属性。

## 手工构造的边界情形个数。**不要放在 ukb_synth 的 roxygen 块与函数之间** ——
## 那样 `@export` 会挂到这个常量上，ukb_synth 反而不被导出（踩过一次）。
N_BRANCH <- 13L

## 展示用的那一人（论文 Fig 1a 的示例生涯，与 codes/py/fig_concept.py 的
## EX_SEGMENTS 逐段一致）。年龄 = 年份 − 出生年；a1 = NA 表示 ongoing。
## 每一段都在演示一件读图时要知道的事：
##   18–23 / 27–31 / 32–34  4123 → 4122：同一 L3（412），上三层合并而 L4 不合并
##   27–31 → 32–34          同一个码、工时不同：兼职 → 全职（状态与职业码是两个通道）
##   35                     同年一份结束一份开始 → is_transition
##   44                     一份短工夹在长工作中间 → is_parallel
##   48–49                  因病中断（106）；回来后换了码 → dt 重置
##   59                     退休（108）→ career_end = 59，其后是 PAD
## 合成 eid 一律在 9000001 起：真实 eid 是首位 1–6 的 7 位数（上限六百五十万），
## UKB 的 Git 审计工具与每日监测按这个区间扫仓库 —— 合成 id 落在区间里会被当成
## 疑似泄露。**注释里也别写出区间端点的数字**，它们本身就会命中
SYNTH_EID0 <- 9000000L
SHOWCASE <- list(
  eid = 9900001L,
  year_birth = 1950L,
  jobs = data.frame(
    a0    = c(18, 27, 32, 35, 44, 50),
    a1    = c(23, 31, 35, 47, 44, 58),
    soc   = c(4123, 4122, 4122, 6115, 9233, 3211),
    hours = c(38, 20, 37, 38, 10, 42),
    shift = c(0, 0, 0, 2, 0, 2)
  ),
  gaps = data.frame(
    a0   = c(16, 24, 48, 59),
    a1   = c(17, 26, 49, NA),
    code = c(103, 105, 106, 108)
  ),
  ## 0/1/2 = never / sometimes / often；未列出的 agent 为 0
  exposure = list(
    "4123" = list(cigarette = 2),
    "4122" = list(cigarette = 1),
    "6115" = list(hot = 1, dusty = 1, cigarette = 1),
    "9233" = list(dusty = 1, fumes = 1),
    "3211" = list(hot = 1, fumes = 1)
  ),
  ## 答"不知道"（-121）的 agent
  dont_know = list("6115" = c("asbestos", "diesel"), "3211" = "paints")
)

#' Synthetic UK Biobank work-history data
#'
#' Generates a table that looks like a real UK Biobank Category 130 export --
#' same column names (`<Name>__0_<slot>`), same codings (negative exposure
#' codes, `-313` = ongoing) -- so you can try every function in the package
#' without touching real data. It contains no real person, and its eids
#' (9000001 onwards) lie outside the range of real UK Biobank eids.
#'
#' The **last person (eid 9900001) is a showcase career**: education, a
#' clerical job, a family-care break, part-time then full-time work in the same
#' occupation, a move into care work with shift work and a short side job, an
#' illness break, nursing, and retirement at 59. It is the example used by
#' [plot_person()] and on the package home page.
#'
#' The first 13 people are hand-built edge cases, each exercising one rule of
#' the data processing (they are what the package's tests check). Apart from
#' that one rule they are ordinary-looking careers -- several jobs, part-time
#' spells, education and family-care breaks, exposures that change with the
#' occupation -- so their pages in [plot_person()] look like real ones:
#'
#' | # | Case | What it tests |
#' |---|---|---|
#' | 1 | ordinary career with normal start and end years | baseline |
#' | 2 | ongoing job (`end = -313`) | filled with the per-person questionnaire year |
#' | 3 | retirement gap (108), no work afterwards | `career_end_source = "retire"` |
#' | 4 | **works again after retiring** | the first retirement year must not cut the history |
#' | 5 | two jobs in one year, one strictly spanning it | `is_parallel` |
#' | 6 | two jobs in one year, one ending as the other starts | `is_transition` |
#' | 7 | no retirement record, last job long ago | `last_job_plus1` |
#' | 8 | career crosses `age_max = 75` | capped |
#' | 9 | every exposure answered "do not know" (-121) | `peak_*` is NA while `yrs_*` is 0 |
#' | 10 | gaps only, no jobs | the panel is built from jobs AND gaps |
#' | 11 | end year before start year | dropped |
#' | 12 | SOC code outside the vocabulary | treated as unknown |
#' | 13 | decades with no record inside the window | `coverage < 0.5`, `flag_incomplete = 1` |
#'
#' **Everyone else is drawn to resemble the real UK Biobank cohort.** Each
#' random person is given a sex and one of the reference career types of the
#' accompanying paper (12 for women, 9 for men, in their observed proportions),
#' and their history is generated from that type's aggregate profile: the
#' occupations it contains (four-digit SOC 2000, and job-to-job moves only
#' along code pairs actually observed), when careers start and end, how often
#' and why they are interrupted (family care, marginal work, education,
#' unemployment, illness), retirement age, part-time work, long hours, night
#' shifts, the ten workplace exposures and how often each is answered "do not
#' know". As a result a sample of a few thousand reproduces the reference
#' cohort's headline figures -- median end of working life 58 (women) and 60
#' (men), about half ending in retirement, 34 and 40 working years, women's
#' long family-care breaks, men's manual-work exposures -- closely enough for
#' realistic examples and teaching. It is still synthetic: rare combinations,
#' long-range dependencies within a life and associations with health are not
#' modelled, so **never use it to draw substantive conclusions**.
#'
#' The profiles are stored in `system.file("extdata", "synth_calibration.json",
#' package = "ukbcareer")`. They are aggregate statistics only (shares,
#' medians, counts), computed by the package authors under their UK Biobank
#' application: every cell describing fewer than 10 people is set to 0, the
#' counts behind every share are rounded to the nearest 10, code pairs seen
#' fewer than 10 times are dropped, and no individual record or
#' participant identifier is included. `attr(x, "reference_type")` gives the
#' type each random person was drawn from (`NA` for the hand-built people);
#' it is a generating label, not what [ukb_typology()] will find in the sample.
#'
#' @param n Total number of people (at least 13). With `n > 13` the last one
#'   is the showcase career.
#' @param seed Random seed.
#' @param max_slots Number of job and gap slots (real exports have 40 / 33).
#'   Histories with more jobs or breaks than this are cut at the last slot.
#' @return A `data.table` in the layout of a UK Biobank export, ready for
#'   [ukb_worklife()]. `attr(x, "branches")` names the edge cases,
#'   `attr(x, "showcase_eid")` gives the eid of the showcase career and
#'   `attr(x, "reference_type")` the reference type behind each random person.
#' @examples
#' d <- ukb_synth(n = 40)
#' wl <- ukb_worklife(d, verbose = FALSE)
#' wl$cohort[1:5, .(eid, career_end, career_end_source)]
#' @export
ukb_synth <- function(n = 200L, seed = 42L, max_slots = 12L) {
  if (n < N_BRANCH) {
    stop("n must be at least ", N_BRANCH, " (the first ", N_BRANCH,
         " people are hand-built boundary cases)")
  }
  set.seed(seed)
  n <- as.integer(n)
  ns <- as.integer(max_slots)

  ## 每人：出生年、性别、录入年
  n_rand <- max(n - N_BRANCH, 0L)
  yb <- c(1940L, 1945L, 1948L, 1950L, 1952L, 1955L, 1958L, 1935L, 1960L,
          1947L, 1951L, 1953L, 1942L, rep(NA_integer_, n_rand))
  sex <- c(0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L,
           rep(NA_integer_, n_rand))
  entry <- rep(2015L, n)
  entry[c(2L, 5L)] <- 2016L

  ## 空槽矩阵
  mk <- function(fill = NA_real_) matrix(fill, nrow = n, ncol = ns)
  jstart <- mk(); jend <- mk(); soc <- mk(); hcat <- mk(); hex <- mk()
  shift_any <- mk(); shift_mix <- mk(); shift_night <- mk(); breath <- mk()
  expo <- lapply(EXPOSURES, function(...) mk())
  names(expo) <- EXPOSURES
  gcode <- mk(); gstart <- mk(); gend <- mk()

  ## 边界情形用的 SOC 码（取自真实 SOC2000 的几个大类）
  socs <- c(1121, 2211, 2314, 3211, 4122, 5231, 6121, 7111, 8211, 9139)

  ## exp_lv：标量（10 个 agent 同级）或长度 10 的向量（0/1/2，NA = 答"不知道"）。
  ## hc 非 NA 时写分档工时（22604）而不写精确工时 —— 真实导出里二者逐槽互斥。
  add_job <- function(i, k, s, e, code, hours = 35, sh = 0, exp_lv = 0,
                      hc = NA_real_, br = 0) {
    jstart[i, k] <<- s; jend[i, k] <<- e; soc[i, k] <<- code
    if (is.na(hc)) hex[i, k] <<- hours else hcat[i, k] <<- hc
    shift_any[i, k] <<- if (sh > 0) 1 else 0
    if (sh == 2) shift_night[i, k] <<- 1 else shift_night[i, k] <<- 9
    shift_mix[i, k] <<- 9
    breath[i, k] <<- br
    lv <- rep_len(exp_lv, length(EXPOSURES))
    code_lv <- c(0, -131, -141)[lv + 1L]
    code_lv[is.na(lv)] <- CODINGS$exposure_dk
    for (a in seq_along(EXPOSURES)) expo[[EXPOSURES[a]]][i, k] <<- code_lv[a]
  }
  add_gap <- function(i, k, s, e, code) {
    gstart[i, k] <<- s; gend[i, k] <<- e; gcode[i, k] <<- code
  }

  ## ── 1..13：手工构造的边界情形 ──────────────────────────────
  ## 每人只测一条规则（行末注释），但**都穿成一份像样的生涯**：几份工作、兼职/
  ## 全职、照护或教育中断、随职业变的暴露与倒班 —— 用户最先看到的就是这 13 人
  ## （README 的 `ukb_report(res, eid = res$eid[1:20])`），一条蓝带配全黄的暴露
  ## 会让人以为包画不出东西。**加料不能碰被测的那条规则**，改前先看 test-etl.R。
  ## 这一段不抽随机数（test-synth 要求 1..13 与 seed、n 无关）。
  lv <- function(...) {                   # 暴露级别：未列出的 agent 为 0，NA = 答"不知道"
    v <- stats::setNames(rep(0, length(EXPOSURES)), EXPOSURES)
    a <- c(...)
    v[names(a)] <- a
    unname(v)
  }
  G <- c(marginal = 101, other = 102, education = 103, family = 105,
         health = 106, unemployed = 107)
  # #1 普通生涯：读书 → 文员 → 照护 → 回来当护士，先兼职后全职带夜班
  add_gap(1L, 1L, 1956, 1958, G[["education"]])
  add_job(1L, 1L, 1959, 1963, 4122, hours = 38, exp_lv = lv(cigarette = 1))
  add_gap(1L, 2L, 1964, 1970, G[["family"]])
  add_job(1L, 2L, 1971, 1979, 3211, hours = 20, exp_lv = lv(cigarette = 1))
  add_job(1L, 3L, 1980, 2000, 3211, hours = 37, sh = 2, exp_lv = lv(cigarette = 1, hot = 1))
  # #2 ongoing（录入年 2016）：医学院 → 医生，年轻时长工时夜班
  add_gap(2L, 1L, 1963, 1967, G[["education"]])
  add_job(2L, 1L, 1968, 1979, 2211, hours = 56, sh = 2, exp_lv = lv(cigarette = 2))
  add_job(2L, 2L, 1980, CODINGS$ongoing, 2211, hours = 45, exp_lv = lv(cigarette = 1))
  # #3 退休后无在职年
  add_job(3L, 1L, 1964, 1969, 7111, hours = 40, exp_lv = lv(cigarette = 2, cold = 1))
  add_gap(3L, 1L, 1970, 1976, G[["family"]])
  add_job(3L, 2L, 1977, 1990, 4122, hours = 18, exp_lv = lv(cigarette = 1))
  add_job(3L, 3L, 1991, 2008, 4122, hours = 36)
  add_gap(3L, 2L, 2009, CODINGS$ongoing, CODINGS$gap_retirement)
  # #4 退休后**仍有**在职年：普工 → 汽修 → 退休 → 兼职开车
  add_job(4L, 1L, 1966, 1971, 9139, hours = 44, exp_lv = lv(noisy = 1, dusty = 2, cold = 1))
  add_job(4L, 2L, 1972, 2005, 5231, hours = 45,
          exp_lv = lv(noisy = 2, fumes = 2, dusty = 1, asbestos = 1, paints = 1, diesel = 2))
  add_gap(4L, 1L, 2006, 2008, CODINGS$gap_retirement)
  add_job(4L, 3L, 2009, 2013, 8211, hours = 24, exp_lv = lv(diesel = 2, noisy = 1))
  # #5 真并行：兼职教书期间，1990 年一份严格落在里面的短工
  add_gap(5L, 1L, 1970, 1974, G[["education"]])
  add_job(5L, 1L, 1975, 1980, 2314, hours = 40, exp_lv = lv(noisy = 1, cigarette = 1))
  add_gap(5L, 2L, 1981, 1985, G[["family"]])
  add_job(5L, 2L, 1986, 1995, 2314, hours = 24, exp_lv = lv(noisy = 1))
  add_job(5L, 3L, 1990, 1990, 6121, hours = 8, exp_lv = lv(noisy = 1))
  add_job(5L, 4L, 1996, 2012, 2314, hours = 38, exp_lv = lv(noisy = 1))
  # #6 衔接：店员 1999 年离职、同年开货车（长工时、夜班、柴油尾气）
  add_gap(6L, 1L, 1976, 1977, G[["unemployed"]])
  add_job(6L, 1L, 1978, 1999, 7111, hours = 40, exp_lv = lv(cigarette = 1))
  add_job(6L, 2L, 1999, 2012, 8211, hours = 52, sh = 2,
          exp_lv = lv(diesel = 2, noisy = 1, cold = 1, fumes = 1))
  # #7 末次工作很早（28 岁起在家带孩子，没有退休记录）
  add_gap(7L, 1L, 1974, 1975, G[["education"]])
  add_job(7L, 1L, 1976, 1980, 9139, hours = 39, sh = 1, exp_lv = lv(noisy = 2, dusty = 2, cold = 1))
  add_job(7L, 2L, 1981, 1985, 9139, hours = 20, exp_lv = lv(noisy = 1, dusty = 1))
  add_gap(7L, 2L, 1986, CODINGS$ongoing, G[["family"]])
  # #8 跨过 age_max 75：汽修起家，后来当厂长，一直干到问卷时
  add_job(8L, 1L, 1955, 1964, 5231, hours = 46,
          exp_lv = lv(noisy = 2, fumes = 1, asbestos = 1, diesel = 1, dusty = 1))
  add_job(8L, 2L, 1965, CODINGS$ongoing, 1121, hours = 50, exp_lv = lv(noisy = 1))
  # #9 暴露全答"不知道"（**每一份**工作都全 DK，否则 peak 不再是 NA）
  dk <- rep(NA_real_, length(EXPOSURES))
  add_gap(9L, 1L, 1976, 1979, G[["education"]])
  add_job(9L, 1L, 1980, 1989, 4122, hours = 38, exp_lv = dk)
  add_gap(9L, 2L, 1990, 1996, G[["family"]])
  add_job(9L, 2L, 1997, 2010, 4122, hours = 22, exp_lv = dk)
  # #10 只有 gap：读书 → 失业 → 因病无法工作
  add_gap(10L, 1L, 1963, 1968, G[["education"]])
  add_gap(10L, 2L, 1969, 1978, G[["unemployed"]])
  add_gap(10L, 3L, 1979, 2010, CODINGS$gap_health)
  add_job(11L, 1L, 2000, 1980, socs[3L])                      # 起止颠倒 → 剔除
  # #12 词表外码：未知职业那份工作的暴露与倒班照样有
  add_job(12L, 1L, 1969, 1973, 9139, hours = 42, exp_lv = lv(noisy = 1, dusty = 1))
  add_job(12L, 2L, 1974, 2004, 9999, hours = 42, sh = 1,
          exp_lv = lv(noisy = 2, hot = 1, fumes = 1))
  add_gap(12L, 1L, 2005, CODINGS$ongoing, CODINGS$gap_retirement)
  # #13 窗口内有十几年空白（既无 job 也无 gap 记录）→ coverage < 0.5。
  # **这是 coverage 分母必须是 career_end 的检验点**：用 age_recruit 作分母时
  # 比值系统性 ≥ 1、flag_incomplete 恒为 0，这条规则完全空转。
  # **1964–1997 之间不许加任何记录。**
  add_job(13L, 1L, 1958, 1963, 6121, hours = 40, exp_lv = lv(noisy = 1))            # 16–21 岁
  add_job(13L, 2L, 1998, CODINGS$ongoing, 3211, hours = 16, sh = 2,
          exp_lv = lv(hot = 1))                                                      # 56–73 岁

  ## ── N_BRANCH+1 .. n：按参考生涯类型分层的随机生涯 ─────────────
  ## 参数全部来自 inst/extdata/synth_calibration.json（UKB 参考队列的聚合量），
  ## 逻辑在 R/synth-calib.R。
  type <- rep(NA_integer_, n)
  if (n > N_BRANCH) {
    cal <- .synth_calibration()
    K <- SYNTH_KNOBS
    par <- list(Female = .synth_params(cal, "Female"), Male = .synth_params(cal, "Male"))
    for (i in (N_BRANCH + 1L):n) {
      sex[i] <- as.integer(stats::runif(1L) >= K$p_female)       # 0 = Female
      P <- par[[SEXES[sex[i] + 1L]]]
      ti <- names(P$type_prob)[sample.int(length(P$type_prob), 1L, prob = P$type_prob)]
      type[i] <- as.integer(ti)
      tp <- P$types[[ti]]
      yb[i] <- if (!is.null(P$srv$birth_year)) {
        as.integer(.draw(P$srv$birth_year))
      } else {
        b <- round(stats::rnorm(1L, K$birth_mean[[P$sex]], K$birth_sd))
        as.integer(min(max(b, min(K$birth_range)), max(K$birth_range)))
      }
      entry[i] <- as.integer(names(K$entry_year)[sample.int(length(K$entry_year), 1L,
                                                            prob = K$entry_year)])
      cap <- min(entry[i] - yb[i], DEFAULTS$age_max)
      cr <- .synth_career(P, tp, cap)
      jb <- cr$jobs
      for (k in seq_len(min(nrow(jb), ns))) {
        add_job(i, k, yb[i] + jb$a0[k],
                if (is.na(jb$a1[k])) CODINGS$ongoing else yb[i] + jb$a1[k],
                jb$soc[k], hours = jb$hours[k], sh = jb$sh[k],
                exp_lv = cr$expo[k, ], hc = jb$hcat[k], br = jb$breath[k])
      }
      gp <- cr$gaps
      for (k in seq_len(min(nrow(gp), ns))) {
        add_gap(i, k, yb[i] + gp$a0[k],
                if (is.na(gp$a1[k])) CODINGS$ongoing else yb[i] + gp$a1[k],
                gp$code[k])
      }
    }
  }

  ## ── 最后一人：展示用的完整生涯（论文 Fig 1a 的示例） ──────────
  ## **覆写第 n 人而不是追加一行**：随机段的 RNG 次序与前 N_BRANCH 人都不变，
  ## 测试按下标引用的人不受影响；n 仍是总人数。
  if (n > N_BRANCH) {
    i <- n
    for (m in c("jstart", "jend", "soc", "hcat", "hex", "shift_any",
                "shift_mix", "shift_night", "breath", "gcode", "gstart", "gend")) {
      v <- get(m)
      v[i, ] <- NA_real_
      assign(m, v)
    }
    for (nm in EXPOSURES) expo[[nm]][i, ] <- NA_real_
    yb[i] <- SHOWCASE$year_birth
    sex[i] <- 0L
    entry[i] <- 2015L
    for (k in seq_len(nrow(SHOWCASE$jobs))) {
      j <- SHOWCASE$jobs[k, ]
      add_job(i, k, yb[i] + j$a0, yb[i] + j$a1, j$soc, hours = j$hours,
              sh = j$shift)
      for (nm in names(SHOWCASE$exposure[[as.character(j$soc)]])) {
        lv <- SHOWCASE$exposure[[as.character(j$soc)]][[nm]]
        expo[[nm]][i, k] <- c(0, -131, -141)[lv + 1L]
      }
      for (nm in SHOWCASE$dont_know[[as.character(j$soc)]]) {
        expo[[nm]][i, k] <- CODINGS$exposure_dk
      }
    }
    for (k in seq_len(nrow(SHOWCASE$gaps))) {
      g <- SHOWCASE$gaps[k, ]
      add_gap(i, k, yb[i] + g$a0,
              if (is.na(g$a1)) CODINGS$ongoing else yb[i] + g$a1, g$code)
    }
  }

  ## ── 组装成 UKB 的槽列格式 ─────────────────────────────────
  wide <- function(m, nm) {
    colnames(m) <- paste0(nm, "__0_", seq_len(ncol(m)) - 1L)
    data.table::as.data.table(m)
  }
  out <- data.table::data.table(
    eid = c(SYNTH_EID0 + seq_len(n - (n > N_BRANCH)),
            if (n > N_BRANCH) SHOWCASE$eid),
    Year_of_birth__0_0 = yb,
    Sex__0_0 = sex,
    When_occupational_data_entered__0_0 = paste0(entry, "-06-15"),
    Number_of_jobs_held__0_0 = rowSums(!is.na(jstart)),
    Number_of_gap_periods__0_0 = rowSums(!is.na(gcode))
  )
  out <- cbind(
    out,
    wide(jstart, FIELDS$job_start), wide(jend, FIELDS$job_end),
    wide(soc, FIELDS$soc4), wide(hcat, FIELDS$hours_cat),
    wide(hex, FIELDS$hours_exact), wide(shift_any, FIELDS$shift_any),
    wide(shift_mix, FIELDS$shift_mixed), wide(shift_night, FIELDS$shift_night),
    wide(breath, FIELDS$breath), wide(gcode, FIELDS$gap_code),
    wide(gstart, FIELDS$gap_start), wide(gend, FIELDS$gap_end)
  )
  for (nm in EXPOSURES) out <- cbind(out, wide(expo[[nm]], EXPOSURE_FIELDS[[nm]]))

  attr(out, "branches") <- c(
    "1 baseline", "2 ongoing", "3 retired, no work after",
    "4 retired but works again", "5 truly parallel", "6 transition year",
    "7 last job long ago", "8 crosses age_max", "9 all exposures unknown",
    "10 gaps only", "11 end before start", "12 code outside vocabulary",
    "13 long blank inside window"
  )
  attr(out, "showcase_eid") <- if (n > N_BRANCH) SHOWCASE$eid else NA_integer_
  ## 随机段每人抽到的参考类型（手工的边界情形与展示那人为 NA）。
  ## 这是**生成时用的标签**，不是 ukb_typology() 在这批人上会得到的分区。
  if (n > N_BRANCH) type[n] <- NA_integer_
  attr(out, "reference_type") <- type
  out[]
}
