# 9 状态序列

test_that("状态取值域与优先级（在职 > gap）", {
  wl <- ukb_worklife(ukb_synth(n = 120L, seed = 11L), verbose = FALSE)
  a <- data.table::copy(wl$annual)
  a[, s := ukbcareer:::.derive_state(a)]
  expect_true(all(a$s %in% 1:10))
  # 在职年一定是 full_time(1) 或 part_time(2)，即便同年也有 gap
  expect_true(all(a[in_job == 1L, s] %in% c(1L, 2L)))
  # 退休 gap 年 → 8；因病 → 7
  if (nrow(a[in_job == 0L & gap_code == 108]) > 0L) {
    expect_true(all(a[in_job == 0L & gap_code == 108, s] == 8L))
  }
  if (nrow(a[in_job == 0L & gap_code == 106]) > 0L) {
    expect_true(all(a[in_job == 0L & gap_code == 106, s] == 7L))
  }
})

test_that("工时门槛分全职/兼职", {
  wl <- ukb_worklife(ukb_synth(n = 120L, seed = 11L), verbose = FALSE)
  a <- data.table::copy(wl$annual)
  a[, s := ukbcareer:::.derive_state(a, hours_ft_threshold = 30)]
  w <- a[in_job == 1L & !is.na(hours)]
  expect_true(all(w[hours >= 30, s] == 1L))
  expect_true(all(w[hours < 30, s] == 2L))
})

test_that("ukb_states 返回人 × 年龄的矩阵，右侧 NA 是信息", {
  wl <- ukb_worklife(ukb_synth(n = 120L, seed = 11L), verbose = FALSE)
  st <- ukb_states(wl)
  expect_s3_class(st, "ukbcareer_states")
  expect_equal(nrow(st), data.table::uniqueN(wl$annual$eid))
  expect_equal(as.integer(colnames(st)[1L]), 16L)
  expect_true(anyNA(st))               # career_end 之后是 NA
  expect_equal(length(attr(st, "alphabet")), 10L)
})

test_that("状态字母表与配色一一对应且无重复", {
  expect_equal(length(STATE_NAMES), 10L)
  expect_equal(length(STATE_COLOURS), 10L)
  expect_setequal(names(STATE_COLOURS), STATE_NAMES)
  expect_false(anyDuplicated(STATE_COLOURS) > 0L)
})

test_that("gap 码到状态的映射覆盖 config 里的全部 gap 码", {
  expect_setequal(as.numeric(names(GAP_TO_STATE)), CODINGS$gap_codes)
  expect_true(all(GAP_TO_STATE %in% 1:10))
})

test_that("ukb_states 也接受 ukb_career() 的结果", {
  d <- ukb_synth(n = 60L, seed = 12L)
  res <- ukb_career(d, verbose = FALSE)
  st <- ukb_states(res)
  expect_s3_class(st, "ukbcareer_states")
  expect_equal(nrow(st), data.table::uniqueN(attr(res, "worklife")$annual$eid))
})

test_that("结果的派生汇总表按普通 data.table 打印", {
  res <- ukb_career(ukb_synth(n = 60L, seed = 13L), verbose = FALSE)
  out <- utils::capture.output(print(res))
  expect_match(out[1L], "^<ukbcareer_result>")
  agg <- res[, .N, by = career_end_source]
  out2 <- utils::capture.output(print(agg))
  expect_false(any(grepl("ukbcareer_result|Null data.table", out2)))
  expect_true(any(grepl("career_end_source", out2)))
  expect_s3_class(agg, "ukbcareer_result")   # 打印不改变对象本身
})
