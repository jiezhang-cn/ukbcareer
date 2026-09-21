# Tier 0 的 ETL —— 每条边界分支一个测试
# 合成假人的前 13 个人是手工构造的，每人对应一条分支（见 ?ukb_synth）。

wl_fixture <- function(n = 120L) {
  ukb_worklife(ukb_synth(n = n, seed = 7L), verbose = FALSE)
}

test_that("ETL 跑通并返回五张表", {
  wl <- wl_fixture()
  expect_s3_class(wl, "ukbcareer_worklife")
  expect_true(all(c("jobs", "gaps", "annual", "cohort", "exposure") %in% names(wl)))
  expect_equal(nrow(wl$cohort), 120L)
  expect_false(anyDuplicated(wl$cohort$eid) > 0L)
})

test_that("ongoing(-313) 被填成逐人的问卷录入年而不是常数", {
  wl <- wl_fixture()
  co <- wl$cohort
  # #2 是 ongoing 的人，录入年 2016
  e2 <- co$eid[2L]
  expect_equal(co[eid == e2, entry_year], 2016)
  last_age <- max(wl$annual[eid == e2, age])
  expect_lte(last_age, co[eid == e2, entry_year - year_birth])
})

test_that("career_end 的三项与来源标签", {
  wl <- wl_fixture()
  co <- wl$cohort
  expect_true(all(co$career_end_source %in%
                    c("retire", "last_job_plus1", "cap")))
  # #3：退休后无在职年 → source = retire，且 retire_age 有值
  expect_equal(co[3L, career_end_source], "retire")
  expect_false(is.na(co[3L, retire_age]))
  # #4：退休后**仍有**在职年 → 不能停在首个退休年
  expect_gt(co[4L, career_end], 2006 - co[4L, year_birth])
  # #7：无退休记录、末次工作很早 → last_job_plus1
  expect_equal(co[7L, career_end_source], "last_job_plus1")
})

test_that("observed_retirement 是「有确定性退休记录」，不是「退休决定了 career_end」", {
  wl <- wl_fixture()
  co <- wl$cohort
  # 两者**不同**：一个人可能有退休记录，但 last_job_plus1 或 cap 更小，
  # 于是 source != "retire" 而他仍然是被观察到退休的。参考队列上前者 56.8%
  # 而后者约 52%，差的就是这批人。这个量是 V5 验收门槛，口径不能错。
  expect_equal(co$observed_retirement, as.integer(!is.na(co$retire_age)))
  # source == retire 的人一定有退休年（反向不成立）
  expect_true(all(co[career_end_source == "retire", observed_retirement] == 1L))
  # 且确实存在"有退休记录但 source 不是 retire"的人 —— 否则这个测试没在测东西
  expect_gt(sum(co$observed_retirement == 1L &
                  co$career_end_source != "retire"), 0L)
  # 有退休记录时，career_end 不会晚于退休年
  expect_true(all(co[observed_retirement == 1L, career_end <= retire_age]))
})

test_that("面板由 job 与 gap 共同构建（gap 人年不为零）", {
  wl <- wl_fixture()
  expect_gt(mean(wl$annual$in_gap == 1L), 0)
  # 只有 gap 的人（#10）必须在面板里，且 in_job 全 0
  e10 <- wl$cohort$eid[10L]
  expect_gt(nrow(wl$annual[eid == e10]), 0L)
  expect_true(all(wl$annual[eid == e10, in_job] == 0L))
})

test_that("每个人年至少属于 job 或 gap 之一", {
  wl <- wl_fixture()
  expect_true(all(wl$annual$in_job == 1L | wl$annual$in_gap == 1L))
})

test_that("真并行与衔接年是互斥的两类", {
  wl <- wl_fixture()
  a <- wl$annual
  expect_equal(sum(a$is_parallel == 1L & a$is_transition == 1L), 0L)
  # #5 有一份工作严格跨过该年 → 真并行
  e5 <- wl$cohort$eid[5L]
  expect_gte(nrow(a[eid == e5 & is_parallel == 1L]), 1L)
  # #6 两份工作在同年首尾相接 → 衔接
  e6 <- wl$cohort$eid[6L]
  expect_gte(nrow(a[eid == e6 & is_transition == 1L]), 1L)
  expect_equal(nrow(a[eid == e6 & is_parallel == 1L]), 0L)
})

test_that("age_max 是绑定约束", {
  wl <- wl_fixture()
  expect_lte(max(wl$annual$age), 75L)
  expect_lte(max(wl$cohort$career_end, na.rm = TRUE), 75)
})

test_that("起止年颠倒的 job 被剔除", {
  wl <- wl_fixture()
  e11 <- wl$cohort$eid[11L]
  expect_equal(nrow(wl$jobs[eid == e11]), 0L)
  expect_true(all(wl$jobs$end >= wl$jobs$start, na.rm = TRUE))
})

test_that("coverage 的分母是 career_end（不会系统性 >= 1）", {
  wl <- wl_fixture()
  expect_equal(sum(wl$cohort$coverage > 1, na.rm = TRUE), 0L)
  # #13 窗口内有大段空白 → coverage < 0.5 且被标记
  expect_lt(wl$cohort[13L, coverage], 0.5)
  expect_equal(wl$cohort[13L, flag_incomplete], 1L)
  # #7 窗口已被缩短但窗口内是满的 → **不该**被标记
  expect_equal(wl$cohort[7L, flag_incomplete], 0L)
})

test_that("暴露汇总：mean_* 在 [0,2]，DK 时 peak 为 NA 而 yrs 为 0", {
  wl <- wl_fixture()
  ex <- wl$exposure
  for (a in EXPOSURES) {
    m <- ex[[paste0("mean_", a)]]
    expect_true(all(m >= 0 & m <= 2, na.rm = TRUE), label = paste("mean_", a))
  }
  e9 <- wl$cohort$eid[9L]          # 暴露全答 DK
  expect_true(is.na(ex[eid == e9, peak_noisy]))
  expect_equal(ex[eid == e9, yrs_exp_noisy], 0)
})

test_that("缺 field 22500 会 warn 且落到常数回落", {
  d <- ukb_synth(n = 20L, seed = 3L)
  d[, When_occupational_data_entered__0_0 := NULL]
  expect_warning(wl <- ukb_worklife(d, verbose = FALSE), "22500")
  expect_equal(attr(wl, "ukbcareer")$entry_year_source, "fallback_constant")
})

test_that("缺性别列直接报错（分性别建模是硬约束）", {
  d <- ukb_synth(n = 20L, seed = 3L)
  d[, Sex__0_0 := NULL]
  expect_error(ukb_worklife(d, verbose = FALSE), "Sex column not found")
})

test_that("eid 重复直接报错", {
  d <- ukb_synth(n = 20L, seed = 3L)
  d$eid[2L] <- d$eid[1L]
  expect_error(ukb_worklife(d, verbose = FALSE), "duplicates")
})
