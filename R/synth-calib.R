# ══════════════════════════════════════════════════════════════════
# ukb_synth() 随机段的校准：按参考生涯类型分层生成一个人的生涯
# ══════════════════════════════════════════════════════════════════
# 参数来自 inst/extdata/synth_calibration.json —— **只有聚合量**（比例、中位数、
# 计数；n<10 的格置 0，SOC 转移只留 n>=10 的对），由 rpkg/dev/build_synth_calibration.R
# 从论文的参考表构建。JSON 里若有 `server` 块（codes/py/synth_calib_export.py 在服务器上
# 补算的工时、首份工作年龄、中断时长等分布），优先用它；没有时退回下面的默认值。
#
# 生成顺序（逐人）：性别 → 参考类型 → 出生年、录入年 → 生涯怎么结束
# （退休 / 问卷时仍在职 / 末次工作后无记录）→ 首份工作 → 中断 → 把在职年切成若干份工作 →
# 每份工作的 SOC、工时、轮班、十种暴露、"不知道"。

## 调参旋钮。数值由 rpkg/dev/validate_synth.R 对照参考队列的汇总量定出，
## 改动后须重跑那个脚本。
SYNTH_KNOBS <- list(
  p_female = 0.558,                 # 参考队列 65,639 / 117,588
  birth_mean = c(Female = 1952, Male = 1950), birth_sd = 7.5,
  birth_range = 1937:1970,          # 招募时 40-69 岁（2006-10）
  entry_year = c("2015" = 0.60, "2016" = 0.30, "2017" = 0.10),
  ## 退休年龄 = 两段混合：一部分提前退休（均值 54），其余围绕本型的退休中位数
  retire_shift = c(Female = 2.5, Male = 4), retire_sd = c(Female = 4, Male = 5),
  p_early_retire = c(Female = 0.15, Male = 0.22), early_retire = c(54, 4),
  p_early = c(Female = 0.06, Male = 0.015),   # 很早离开劳动力、此后再无工作记录
  p_retire = c(Female = 0.92, Male = 0.96),   # 问卷前离开工作的人里有退休记录的比例
  p_pregap = 0.04,                  # 退休前夹一段失业/照护/因病 → career_end 来自末次工作
  p_unretire = 0.03,                # 退休后又工作过（参考队列 3.3%）
  ## 首份工作年龄的默认分布只在型的年龄构成缺失时用；平时取自 P1（见 .start_ages）
  first_job_age = list(k = 15:28, p = c(.20, .22, .09, .10, .07, .06, .07, .06,
                                        .04, .03, .02, .02, .01, .01)),
  p_edu_start = 0.25,               # 18 岁以后才开始工作的人里，先报了一段教育（103）
  jobs = list(k = 1:10, p = c(.30, .19, .14, .11, .08, .06, .05, .03, .02, .02)),
  lag = c("0" = 0.62, "1" = 0.22, "2" = 0.10, "3" = 0.06),  # 下一份起始年 − 上一份结束年
  same_code = 0.45, via_transition = 0.70,
  p_side = 0.18,                    # 有一份嵌在长工作里的副业
  exact_hours = 0.55,               # 精确工时（22605）而非分档（22604）
  p_pt = c(Female = 0.07, Male = 0.015), p_pt_after_care = 0.50, p_pt_late = 0.20,
  p_long_base = c(Female = 0.05, Male = 0.12),
  p_night_base = c(Female = 0.03, Male = 0.05), p_shift_day = 0.07,
  p_dk = c(Female = 0.17, Male = 0.14),
  dk_weight = c(noisy = 1, cold = 1, hot = 1, dusty = 1, fumes = 1.5, cigarette = 1,
                asbestos = 3, paints = 1.5, pesticides = 2, diesel = 2.5),
  ## 中断：时长的中位数（年）与起始年龄（均值, 标准差），按 gap 码
  break_years = list(Female = c("101" = 3, "102" = 2, "103" = 2, "105" = 6, "106" = 2, "107" = 1.5),
                     Male   = c("101" = 3, "102" = 2, "103" = 2, "105" = 2, "106" = 2, "107" = 1.5)),
  break_start = list("101" = c(32, 8), "102" = c(35, 10), "103" = c(24, 5),
                     "105F" = c(29, 5), "105M" = c(35, 10), "106" = c(42, 9),
                     "107" = c(33, 10)),
  edu_at_start_rate = 0.15          # 期望的"开头那段教育"人均段数，从 103 的份额里扣掉
)

.synth_cache <- new.env(parent = emptyenv())

#' 读包内的校准 JSON（一次会话只读一次）
#' @noRd
.synth_calibration <- function() {
  if (is.null(.synth_cache$cal)) {
    f <- system.file("extdata", "synth_calibration.json", package = "ukbcareer")
    if (!nzchar(f)) stop("synth_calibration.json is missing from the installed package -- reinstall ukbcareer")
    .synth_cache$cal <- jsonlite::fromJSON(f, simplifyVector = TRUE)
  }
  .synth_cache$cal
}

## 从 {k, frac} 抽一个；d 缺失或全 0 时用默认分布
.draw <- function(d, k = NULL, p = NULL) {
  if (!is.null(d) && length(d$k) && sum(d$frac) > 0) {
    k <- d$k; p <- d$frac
  }
  k[sample.int(length(k), 1L, prob = p)]
}

## P1 的 ended 在 16–35 岁是"序列还没开始"：1 − ended 的逐年增量就是起始年龄的分布。
## 16 岁那一档包含所有 16 岁以前开始的人（面板从 age_min = 16 起）
.start_ages <- function(ended, ages) {
  if (is.null(ended) || length(ended) != length(ages)) return(NULL)
  w <- ages <= 35
  inc <- diff(c(0, 1 - ended[w]))
  inc[inc < 0] <- 0
  if (sum(inc) <= 0) return(NULL)
  k <- ages[w]
  ## 16 岁那一档一部分分给 14–15 岁（参考队列里不少人 15 岁开始工作）
  list(k = c(15L, k), frac = c(inc[1L] * 0.5, inc[1L] * 0.5, inc[-1L]))
}

## 中断时长：对数正态，中位数 med，下限 1 年
.draw_years <- function(med, sdlog = 0.7) {
  max(1L, as.integer(round(stats::rlnorm(1L, log(med), sdlog))))
}

#' 每个性别一次：把 JSON 整理成生成器直接用的形状
#' @noRd
.synth_params <- function(cal, sex) {
  K <- SYNTH_KNOBS
  x <- cal$sexes[[sex]]
  srv <- x$server
  fem <- sex == "Female"
  tr <- x$transitions
  ages <- x$ages
  types <- lapply(names(x$types), function(k) {
    t <- x$types[[k]]
    wy <- max(t$work_years_median, 1)
    mix <- unlist(t$breaks$mix)
    if (!length(mix) || sum(mix) <= 0) {
      mix <- c("101" = .2, "102" = .03, "103" = .2, "105" = .4, "106" = .03, "107" = .1)
    }
    ## 期望段数按码拆开，扣掉"开头那段教育"（另行生成）
    e <- t$breaks$per_person * mix / sum(mix)
    e[["103"]] <- max(e[["103"]] - K$edu_at_start_rate, 0)
    list(
      soc = as.integer(t$soc4$code), soc_p = t$soc4$frac,
      p2 = unlist(t$exposure$peak)[EXPOSURES],
      m = unlist(t$exposure$mean)[EXPOSURES],
      p_long = if (t$long_hours_years_median > 0) {
        min(t$long_hours_years_median / wy, 0.9)
      } else K$p_long_base[[sex]],
      p_night = if (t$night_shift_years_median > 0) {
        min(t$night_shift_years_median / wy, 0.85)
      } else K$p_night_base[[sex]],
      brk_lambda = sum(e), brk_code = as.integer(names(e)), brk_p = e,
      retire_med = t$retire$age_median,
      start = .start_ages(t$chron$ended, ages),
      srv = srv$types[[k]]
    )
  })
  names(types) <- names(x$types)
  list(sex = sex, fem = fem, type_prob = unlist(x$type_prob), types = types,
       tr_to = tr$to, tr_n = tr$n, tr_idx = split(seq_along(tr$from), tr$from),
       srv = srv)
}

## 下一份工作的 SOC：同码 / 沿观察到的转移 / 从本型的构成里重抽
.next_soc <- function(P, tp, cur) {
  K <- SYNTH_KNOBS
  if (!is.na(cur)) {
    same <- P$srv$job_sequence$same_code_next_frac %||% K$same_code
    if (stats::runif(1L) < same) return(cur)
    idx <- P$tr_idx[[as.character(cur)]]
    if (length(idx) && stats::runif(1L) < K$via_transition) {
      return(P$tr_to[idx[sample.int(length(idx), 1L, prob = P$tr_n[idx])]])
    }
  }
  tp$soc[sample.int(length(tp$soc), 1L, prob = tp$soc_p)]
}

## 工时：返回 list(hours, hcat)。分档时 hours 为 NA、hcat 为 UKB 码
.draw_hours <- function(P, tp, pt) {
  K <- SYNTH_KNOBS
  h <- P$srv$hours
  exact <- stats::runif(1L) < (h$exact_frac %||% K$exact_hours)
  if (pt) {
    if (exact) return(list(hours = sample(c(8, 10, 12, 15, 16, 18, 20, 20, 21, 22.5,
                                            24, 25, 25, 27.5), 1L), hcat = NA_real_))
    return(list(hours = NA_real_, hcat = sample(c(-1520, -2030), 1L, prob = c(.4, .6))))
  }
  long <- stats::runif(1L) < tp$p_long
  if (exact) {
    hv <- if (long) sample(c(48, 50, 50, 55, 60, 60, 65, 70), 1L) else
      sample(c(30, 32, 35, 35, 36, 37, 37, 37.5, 38, 39, 40, 40, 42, 45), 1L)
    return(list(hours = hv, hcat = NA_real_))
  }
  ## 分档里 "40 小时及以上" 记作 48 小时 → 就是长工时
  list(hours = NA_real_, hcat = if (long) 4000 else -3040)
}

#' 一个人的生涯（年龄尺度）
#'
#' @return list(jobs = data.frame(a0, a1, soc, hours, hcat, sh, breath),
#'   expo = 工作 × 10 的矩阵（0/1/2，NA = 不知道）, gaps = data.frame(a0, a1, code))。
#'   `a1 = NA` 表示问卷时仍在进行（写出时为 -313）。
#' @noRd
.synth_career <- function(P, tp, cap) {
  K <- SYNTH_KNOBS
  sx <- P$sex
  gaps <- list()
  addg <- function(a0, a1, code) gaps[[length(gaps) + 1L]] <<- c(a0, a1, code)

  ## 1) 首份工作：型内"面板从几岁开始"的分布（P1 里 ended 在低龄段的下降 = 开始）
  f <- if (!is.null(tp$srv$first_job_age)) {
    .draw(tp$srv$first_job_age)
  } else if (!is.null(tp$start)) {
    .draw(tp$start)
  } else {
    .draw(NULL, K$first_job_age$k, K$first_job_age$p)
  }
  f <- max(as.integer(f), 14L)

  ## 2) 生涯怎么结束
  ## "要是一直观察下去会在几岁离开工作"。**不用服务器的 retire_age 分布**：那是
  ## 问卷前已退休者的年龄，被录入年右截断，拿来给所有人抽会把退休占比推高。
  r <- if (stats::runif(1L) < K$p_early_retire[[sx]]) {
    round(stats::rnorm(1L, K$early_retire[1L], K$early_retire[2L]))
  } else {
    round(stats::rnorm(1L, tp$retire_med + K$retire_shift[[sx]], K$retire_sd[[sx]]))
  }
  r <- min(max(r, 45), 74)
  early <- stats::runif(1L) < K$p_early[[sx]]
  if (early) r <- sample(min(f + 3L, 49L):49L, 1L)
  ongoing <- r > cap
  retired_at <- NA
  if (ongoing) {
    last <- cap
  } else if (!early && stats::runif(1L) < K$p_retire[[sx]]) {
    last <- r - 1L
    if (stats::runif(1L) < K$p_pregap && r - f > 8) {
      pg <- sample(1:3, 1L)
      last <- r - pg - 1L
      addg(last + 1L, r - 1L, sample(c(107, 106, 105), 1L, prob = c(.5, .3, .2)))
    }
    addg(r, NA, CODINGS$gap_retirement)
    retired_at <- r
  } else {
    last <- r - 1L
    if (stats::runif(1L) < 0.6) {
      addg(r, NA, if (P$fem) sample(c(105, 106, 107, -717), 1L, prob = c(.6, .2, .1, .1))
           else sample(c(106, 107, 105, -717), 1L, prob = c(.4, .35, .1, .15)))
    }
  }
  f <- min(f, last)

  ## 3) 开头的教育
  if (f >= 18L && stats::runif(1L) < K$p_edu_start) {
    addg(max(16L, f - sample(1:4, 1L)), f - 1L, 103)
  }

  ## 4) 生涯中段的中断
  brk <- NULL
  nb <- min(stats::rpois(1L, tp$brk_lambda), 4L)
  if (nb > 0L && last - f >= 4L) {
    codes <- tp$brk_code[sample.int(length(tp$brk_code), nb, replace = TRUE,
                                    prob = tp$brk_p)]
    for (cd in codes) {
      key <- as.character(cd)
      bc <- tp$srv$breaks$by_code[[key]]
      st <- if (!is.null(bc$start_age)) c(bc$start_age$p50,
                                          max((bc$start_age$p75 - bc$start_age$p25) / 1.35, 2))
            else K$break_start[[if (cd == 105) paste0(key, if (P$fem) "F" else "M") else key]]
      med <- if (!is.null(bc$years)) max(bc$years$p50, 1) else K$break_years[[sx]][[key]]
      a0 <- as.integer(round(stats::rnorm(1L, st[1L], st[2L])))
      a0 <- min(max(a0, f + 1L), last - 1L)
      brk <- rbind(brk, c(a0, a0 + .draw_years(med) - 1L, cd))
    }
    brk <- brk[order(brk[, 1L]), , drop = FALSE]
    ## 不重叠、不越过末次在职年（最后一年总是在职）
    keep <- logical(nrow(brk))
    nxt <- f + 1L
    for (b in seq_len(nrow(brk))) {
      brk[b, 1L] <- max(brk[b, 1L], nxt)
      brk[b, 2L] <- min(brk[b, 2L], last - 1L)
      keep[b] <- brk[b, 2L] >= brk[b, 1L]
      if (keep[b]) nxt <- brk[b, 2L] + 2L
    }
    brk <- brk[keep, , drop = FALSE]
    for (b in seq_len(nrow(brk))) addg(brk[b, 1L], brk[b, 2L], brk[b, 3L])
  }

  ## 5) 在职的年段 = [f, last] 扣掉中段中断
  spans <- list(); s0 <- f
  for (b in seq_len(NROW(brk))) {
    if (brk[b, 1L] > s0) spans[[length(spans) + 1L]] <- c(s0, brk[b, 1L] - 1L)
    s0 <- brk[b, 2L] + 1L
  }
  if (last >= s0) spans[[length(spans) + 1L]] <- c(s0, last)

  ## 6) 切成若干份工作：先在每个年段内随机选切点，再定相邻两份的衔接
  J <- .draw(tp$srv$jobs_per_person, K$jobs$k, K$jobs$p)
  cand <- unlist(lapply(spans, function(s) if (s[2L] > s[1L]) s[1L]:(s[2L] - 1L)))
  n_cut <- min(max(J - length(spans), 0L), length(cand))
  cuts <- if (n_cut > 0L) sort(cand[sample.int(length(cand), n_cut)]) else integer(0)
  lagp <- K$lag
  ls <- P$srv$job_sequence$next_start_minus_end
  if (!is.null(ls) && length(ls$k)) {
    lagp <- c("0" = sum(ls$frac[ls$k <= 0]), "1" = sum(ls$frac[ls$k == 1]),
              "2" = sum(ls$frac[ls$k == 2]), "3" = sum(ls$frac[ls$k >= 3]))
  }
  pieces <- list()
  for (s in spans) {
    ends <- c(cuts[cuts >= s[1L] & cuts < s[2L]], s[2L])
    a0 <- s[1L]
    for (e in ends) {
      pieces[[length(pieces) + 1L]] <- c(min(a0, e), e)
      if (e < s[2L]) {
        lag <- as.integer(names(lagp)[sample.int(length(lagp), 1L, prob = lagp)])
        ## lag ≥ 2 留下没有任何记录的年份（参考队列 coverage 中位数约 0.94）
        a0 <- min(e + lag, s[2L])
      }
    }
  }

  ## 7) 每份工作的内容
  first_care <- min(c(Inf, vapply(gaps, function(g) if (g[3L] == 105) g[1L] else Inf, 0)))
  nj <- length(pieces)
  jobs <- data.frame(a0 = vapply(pieces, `[`, 0, 1L), a1 = vapply(pieces, `[`, 0, 2L),
                     soc = NA_real_, hours = NA_real_, hcat = NA_real_, sh = 0, breath = 0)
  cur <- NA
  for (k in seq_len(nj)) {
    cur <- .next_soc(P, tp, cur)
    jobs$soc[k] <- cur
    ppt <- K$p_pt[[sx]]
    if (jobs$a0[k] > first_care) ppt <- K$p_pt_after_care
    if (jobs$a0[k] >= 55L) ppt <- max(ppt, K$p_pt_late)
    hh <- .draw_hours(P, tp, stats::runif(1L) < ppt)
    jobs$hours[k] <- hh$hours; jobs$hcat[k] <- hh$hcat
  }

  ## 副业：严格嵌在一份较长工作的内部 → is_parallel
  side_p <- P$srv$job_sequence$side_job$person_frac %||% K$p_side
  long_k <- which(jobs$a1 - jobs$a0 >= 4)
  if (length(long_k) && stats::runif(1L) < side_p) {
    h <- long_k[sample.int(length(long_k), 1L)]
    y0 <- jobs$a0[h] + sample.int(jobs$a1[h] - jobs$a0[h] - 2L, 1L)
    y1 <- min(y0 + sample(0:5, 1L), jobs$a1[h] - 1L)
    jobs <- rbind(jobs, data.frame(a0 = y0, a1 = y1, soc = .next_soc(P, tp, NA),
                                   hours = sample(c(6, 8, 10, 12, 15), 1L),
                                   hcat = NA_real_, sh = 0, breath = 0))
  }
  ## 退休后又回去做了一阵（兼职）→ 首个退休年不是 career_end
  if (!is.na(retired_at) && retired_at + 4L < cap && stats::runif(1L) < K$p_unretire) {
    jobs <- rbind(jobs, data.frame(a0 = retired_at + 1L, a1 = retired_at + sample(1:3, 1L),
                                   soc = jobs$soc[nj], hours = sample(c(12, 16, 20), 1L),
                                   hcat = NA_real_, sh = 0, breath = 0))
  }
  jobs <- jobs[order(jobs$a0, jobs$a1), , drop = FALSE]
  nj <- nrow(jobs)

  ## 8) 轮班与暴露：同一个码的工作共用一套（同一份职业，同一种工作环境）
  codes <- unique(jobs$soc)
  Jc <- length(codes)
  q2 <- 1 - (1 - pmin(tp$p2, 0.99))^(1 / Jc)
  q1 <- pmin(pmax(tp$m - 2 * q2, 0), 1 - q2)
  expo <- matrix(0L, nj, length(EXPOSURES), dimnames = list(NULL, EXPOSURES))
  prof <- list()
  for (cd in as.character(codes)) {
    fct <- stats::rgamma(1L, shape = 2, rate = 2)          # 工作层面的"脏累程度"，让 agent 共现
    p2 <- pmin(q2 * fct, 0.95); p1 <- pmin(q1 * fct, 1 - p2)
    u <- stats::runif(length(EXPOSURES))
    lv <- ifelse(u < p2, 2L, ifelse(u < p2 + p1, 1L, 0L))
    sh <- if (stats::runif(1L) < tp$p_night) 2 else if (stats::runif(1L) < K$p_shift_day) 1 else 0
    prof[[cd]] <- list(lv = lv, sh = sh)
  }
  dk_rate <- P$srv$dk$agent_rate
  dk_w <- if (!is.null(dk_rate)) unlist(dk_rate)[EXPOSURES] + 1e-6 else K$dk_weight
  for (k in seq_len(nj)) {
    pr <- prof[[as.character(jobs$soc[k])]]
    expo[k, ] <- pr$lv
    jobs$sh[k] <- pr$sh
    if (stats::runif(1L) < K$p_dk[[sx]]) {
      nd <- .draw(P$srv$dk$n_agents_dk_per_job, 1:3, c(.5, .3, .2))
      nd <- min(max(as.integer(nd), 1L), length(EXPOSURES))
      expo[k, sample.int(length(EXPOSURES), nd, prob = dk_w)] <- NA_integer_
    }
    jobs$breath[k] <- as.numeric(stats::runif(1L) <
                                   0.02 + 0.08 * isTRUE(expo[k, "dusty"] >= 1L))
  }
  if (ongoing) jobs$a1[jobs$a1 == last & jobs$a0 <= last] <- NA

  g <- if (length(gaps)) as.data.frame(do.call(rbind, gaps)) else
    data.frame(V1 = numeric(0), V2 = numeric(0), V3 = numeric(0))
  names(g) <- c("a0", "a1", "code")
  g <- g[order(g$a0), , drop = FALSE]
  list(jobs = jobs, expo = expo, gaps = g)
}
