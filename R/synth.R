# ══════════════════════════════════════════════════════════════════
# 合成假人 —— 测试、示例与绘图 smoke test 的唯一数据源
# ══════════════════════════════════════════════════════════════════
# **真实 UKB 数据不能进包**（PLAN §1）。黄金测试用的 2,000 人输入留在服务器，
# 包内只放这个生成器。它刻意覆盖 ETL 的每一条边界分支 —— 见 `branches` 属性。

## 手工构造的边界情形个数。**不要放在 ukb_synth 的 roxygen 块与函数之间** ——
## 那样 `@export` 会挂到这个常量上，ukb_synth 反而不被导出（踩过一次）。
N_BRANCH <- 13L

#' 生成合成的 UKB 工作史宽表
#'
#' 造出的表在**列名与编码上**与真实 dispensal 同构（`<Name>__0_<slot>` 槽格式、
#' 负数暴露码、`-313` = ongoing），所以它能走完整条 ETL。
#'
#' 前 12 个人是**手工构造的边界情形**，每人对应一条 ETL 分支：
#'
#' | # | 情形 | 测什么 |
#' |---|---|---|
#' | 1 | 单份工作，正常起止 | 基线 |
#' | 2 | ongoing 工作（`end = -313`） | 逐人录入年填充 |
#' | 3 | 退休 gap（108）后无在职年 | `career_end = retire` |
#' | 4 | **退休后仍有在职年** | 细则 1：不能用首个退休年截断 |
#' | 5 | 同年两份工作，其一**严格跨过**该年 | `is_parallel` |
#' | 6 | 同年两份工作，都在该年起止 | `is_transition`（衔接） |
#' | 7 | 无退休记录，末次工作很早 | 细则 3：`last_job_plus1` + `flag_incomplete` |
#' | 8 | 职业生涯跨过 `age_max = 75` | 封顶 |
#' | 9 | 暴露**全答 DK**（-121） | `peak_*` 为 NA 而 `yrs_*` 为 0 |
#' | 10 | 只有 gap 没有 job | 面板必须由 job ∪ gap 共同构建 |
#' | 11 | 起止年颠倒 | 剔除 |
#' | 12 | 词表外的 SOC 码 | UNK |
#' | 13 | 窗口内有十几年空白 | `coverage < 0.5` → `flag_incomplete` |
#'
#' 其余的人随机生成，用来填出可画图的样本量。
#'
#' @param n 总人数（≥ 12）。
#' @param seed 随机种子。
#' @param max_slots job 与 gap 的槽数（真实数据是 40 / 33）。
#' @return 一个 `data.table`，可直接喂给 [ukb_worklife]。
#'   `attr(x, "branches")` 记录前 12 人各测哪条分支。
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

  ## ── 组装成 UKB 的槽列格式 ─────────────────────────────────
  wide <- function(m, nm) {
    colnames(m) <- paste0(nm, "__0_", seq_len(ncol(m)) - 1L)
    data.table::as.data.table(m)
  }
  out <- data.table::data.table(
    eid = 9000000L + seq_len(n),
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
  out[]
}
