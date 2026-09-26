# 合成数据的"像不像"：随机段按参考类型的聚合量生成，头条数字应落在参考队列附近
# （参考值：reference_qc / T1；宽容区间，n = 1500 的抽样误差 + 生成器的近似）

synth_wl <- function() {
  d <- ukb_synth(n = 1500L, seed = 5L)
  list(d = d, wl = ukb_worklife(d, verbose = FALSE))
}

test_that("校准文件在包里，且只有聚合量", {
  cal <- .synth_calibration()
  expect_setequal(names(cal$sexes), c("Female", "Male"))
  expect_equal(length(cal$sexes$Female$types), 12L)
  expect_equal(length(cal$sexes$Male$types), 9L)
  # 转移对只留 n >= 10
  expect_true(all(cal$sexes$Female$transitions$n >= 10))
  expect_true(all(cal$sexes$Male$transitions$n >= 10))
  # 没有 7 位、形如 eid 的整数
  txt <- readLines(system.file("extdata", "synth_calibration.json",
                               package = "ukbcareer"), warn = FALSE)
  expect_false(any(grepl("(^|[^0-9.])[1-6][0-9]{6}([^0-9.]|$)", txt)))
})

test_that("随机段的生涯结束、来源与工作年数接近参考队列", {
  x <- synth_wl()
  co <- x$wl$cohort[!is.na(attr(x$d, "reference_type"))]
  for (s in c("Female", "Male")) {
    c1 <- co[sex == s]
    ref_ce <- c(Female = 58, Male = 60)[[s]]
    expect_lt(abs(stats::median(c1$career_end) - ref_ce), 3, label = paste(s, "career_end"))
    expect_gt(mean(c1$career_end_source == "retire"), 0.38)
    expect_gt(mean(c1$career_end_source == "cap"), 0.25)
    wy <- x$wl$annual[eid %in% c1$eid & in_job == 1L, .N, by = eid]$N
    ref_wy <- c(Female = 34, Male = 40)[[s]]
    expect_lt(abs(stats::median(wy) - ref_wy), 4, label = paste(s, "work years"))
  }
})

test_that("女性的照护中断远多于男性，男性的体力暴露更高", {
  x <- synth_wl()
  co <- x$wl$cohort[!is.na(attr(x$d, "reference_type"))]
  g <- x$wl$gaps[eid %in% co$eid]
  g <- merge(g, co[, .(eid, sex)], by = "eid")
  care <- g[gap_code == 105, .N, by = sex]
  n_by <- co[, .N, by = sex]
  rate <- setNames(care$N / n_by$N[match(care$sex, n_by$sex)], care$sex)
  expect_gt(rate[["Female"]], 3 * rate[["Male"]])
  ex <- merge(x$wl$exposure, co[, .(eid, sex)], by = "eid")
  pk <- ex[, .(noisy = mean(peak_noisy == 2, na.rm = TRUE)), by = sex]
  expect_gt(pk[sex == "Male", noisy], pk[sex == "Female", noisy])
})

test_that("手工边界情形与展示那人不受随机段影响", {
  d1 <- ukb_synth(n = 40L, seed = 1L)
  d2 <- ukb_synth(n = 80L, seed = 99L)
  expect_equal(as.data.frame(d1[1:13]), as.data.frame(d2[1:13]), ignore_attr = TRUE)
  expect_equal(d1$eid[40L], attr(d1, "showcase_eid"))
  expect_true(all(is.na(attr(d1, "reference_type")[c(1:13, 40L)])))
  expect_false(anyNA(attr(d1, "reference_type")[14:39]))
})
