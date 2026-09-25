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
#' without touching real data. It contains no real person.
#'
#' The **last person (eid 9900001) is a showcase career**: education, a
#' clerical job, a family-care break, part-time then full-time work in the same
#' occupation, a move into care work with shift work and a short side job, an
#' illness break, nursing, and retirement at 59. It is the example used by
#' [plot_person()] and on the package home page.
#'
#' The first 13 people are hand-built edge cases, each exercising one rule of
#' the data processing (they are what the package's tests check):
#'
#' | # | Case | What it tests |
#' |---|---|---|
#' | 1 | one job with normal start and end | baseline |
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
#' Everyone else is random, to give a sample large enough to plot.
#'
#' @param n Total number of people (at least 13). With `n > 13` the last one
#'   is the showcase career.
#' @param seed Random seed.
#' @param max_slots Number of job and gap slots (real exports have 40 / 33).
#' @return A `data.table` in the layout of a UK Biobank export, ready for
#'   [ukb_worklife()]. `attr(x, "branches")` names the edge cases and
#'   `attr(x, "showcase_eid")` gives the eid of the showcase career.
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
          1947L, 1951L, 1953L, 1942L,
          sample(1937:1965, n_rand, replace = TRUE))
  sex <- c(0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L,
           sample(0:1, n_rand, replace = TRUE))
  entry <- rep(2015L, n)
  entry[c(2L, 5L)] <- 2016L

  ## 空槽矩阵
  mk <- function(fill = NA_real_) matrix(fill, nrow = n, ncol = ns)
  jstart <- mk(); jend <- mk(); soc <- mk(); hcat <- mk(); hex <- mk()
  shift_any <- mk(); shift_mix <- mk(); shift_night <- mk(); breath <- mk()
  expo <- lapply(EXPOSURES, function(...) mk())
  names(expo) <- EXPOSURES
  gcode <- mk(); gstart <- mk(); gend <- mk()

  ## 常用的 SOC 码（取自真实 SOC2000 的几个大类）
  socs <- c(1121, 2211, 2314, 3211, 4122, 5231, 6121, 7111, 8211, 9139)

  add_job <- function(i, k, s, e, code, hours = 35, sh = 0, exp_lv = 0) {
    jstart[i, k] <<- s; jend[i, k] <<- e; soc[i, k] <<- code
    hex[i, k] <<- hours
    shift_any[i, k] <<- if (sh > 0) 1 else 0
    if (sh == 2) shift_night[i, k] <<- 1 else shift_night[i, k] <<- 9
    shift_mix[i, k] <<- 9
    breath[i, k] <<- 0
    lv <- c(0, -131, -141)[exp_lv + 1L]
    for (nm in EXPOSURES) expo[[nm]][i, k] <<- lv
  }
  add_gap <- function(i, k, s, e, code) {
    gstart[i, k] <<- s; gend[i, k] <<- e; gcode[i, k] <<- code
  }

  ## ── 1..12：手工构造的边界情形 ──────────────────────────────
  add_job(1L, 1L, 1962, 2000, socs[4L])                       # 基线
  add_job(2L, 1L, 1968, CODINGS$ongoing, socs[2L])            # ongoing
  add_job(3L, 1L, 1970, 2008, socs[5L])                       # 退休后无在职年
  add_gap(3L, 1L, 2009, CODINGS$ongoing, CODINGS$gap_retirement)
  add_job(4L, 1L, 1972, 2005, socs[6L])                       # 退休后**仍有**在职年
  add_gap(4L, 1L, 2006, 2008, CODINGS$gap_retirement)
  add_job(4L, 2L, 2009, 2013, socs[4L])
  add_job(5L, 1L, 1975, 1995, socs[3L])                       # 真并行：跨过 1990
  add_job(5L, 2L, 1990, 1990, socs[7L])
  add_job(6L, 1L, 1978, 1999, socs[8L])                       # 衔接：同年起止
  add_job(6L, 2L, 1999, 2012, socs[9L])
  add_job(7L, 1L, 1976, 1985, socs[10L])                      # 末次工作很早
  add_job(8L, 1L, 1955, CODINGS$ongoing, socs[1L])            # 跨过 age_max 75
  add_job(9L, 1L, 1980, 2010, socs[5L])                       # 暴露全 DK
  for (nm in EXPOSURES) expo[[nm]][9L, 1L] <- CODINGS$exposure_dk
  add_gap(10L, 1L, 1979, 2010, CODINGS$gap_health)            # 只有 gap
  add_job(11L, 1L, 2000, 1980, socs[3L])                      # 起止颠倒 → 剔除
  add_job(12L, 1L, 1974, 2004, 9999)                          # 词表外码
  # #13 窗口内有十几年空白（既无 job 也无 gap 记录）→ coverage < 0.5。
  # **这是 coverage 分母必须是 career_end 的检验点**：用 age_recruit 作分母时
  # 比值系统性 ≥ 1、flag_incomplete 恒为 0，这条规则完全空转。
  add_job(13L, 1L, 1958, 1963, socs[7L])                      # 16–21 岁
  add_job(13L, 2L, 1998, CODINGS$ongoing, socs[4L])           # 56–73 岁，中间空 34 年

  ## ── N_BRANCH+1 .. n：随机 ──────────────────────────────────
  if (n > N_BRANCH) for (i in (N_BRANCH + 1L):n) {
    nj <- sample(1:4, 1L, prob = c(.37, .3, .2, .13))   # 36.8% 的人只报 1 个码
    y <- yb[i] + sample(16:22, 1L)
    for (k in seq_len(nj)) {
      dur <- sample(3:22, 1L)
      e <- min(y + dur, 2014L)
      if (k == nj && stats::runif(1L) < .35) e <- CODINGS$ongoing
      add_job(i, k, y, e, sample(socs, 1L),
              hours = sample(c(20, 30, 35, 40, 50), 1L),
              sh = sample(0:2, 1L, prob = c(.7, .2, .1)),
              exp_lv = sample(0:2, 1L, prob = c(.6, .25, .15)))
      y <- (if (identical(e, CODINGS$ongoing)) 2014L else e) + sample(0:3, 1L)
      if (y > 2013L) break
    }
    if (stats::runif(1L) < .5) {
      gs <- yb[i] + sample(50:62, 1L)
      add_gap(i, 1L, gs, CODINGS$ongoing,
              sample(c(CODINGS$gap_retirement, 107L, 105L, 106L), 1L,
                     prob = c(.6, .15, .15, .1)))
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
    eid = c(9000000L + seq_len(n - (n > N_BRANCH)),
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
  out[]
}
