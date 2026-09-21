# UKB 列名解析与编码转换 —— 纯函数，可用手算的用例钉住

test_that("列名解析认 <Name>__<inst>_<slot>", {
  p <- ukbcareer:::.parse_col(c("Year_job_started__0_3", "eid", "a__1_12"))
  expect_equal(p$name, c("Year_job_started", NA, "a"))
  expect_equal(p$inst, c(0L, NA, 1L))
  expect_equal(p$slot, c(3L, NA, 12L))
})

test_that("槽列按槽索引升序，且不因字典序错位", {
  cols <- paste0("X__0_", c(0, 1, 2, 10, 11, 3))
  expect_equal(ukbcareer:::.slot_cols(cols, "X"),
               paste0("X__0_", c(0, 1, 2, 3, 10, 11)))
})

test_that("暴露负数码是实义等级，-121 是 DK", {
  # 0 < -131 < -141，方向不可颠倒
  expect_equal(ukbcareer:::.map_exposure(c(0, -131, -141)), c(0, 1, 2))
  expect_true(is.na(ukbcareer:::.map_exposure(-121)))
  # 按"负数 = 缺失"清洗会毁掉全部暴露 → 这个测试就是防它
  expect_false(anyNA(ukbcareer:::.map_exposure(c(0, -131, -141))))
})

test_that("ongoing(-313) 逐人填充而不是常数", {
  x <- c(2000, -313, -313, 1850)
  fill <- c(2015, 2015, 2016, 2015)
  out <- ukbcareer:::.clean_year(x, ongoing_fill = fill)
  expect_equal(out[1L], 2000)
  expect_equal(out[2L], 2015)
  expect_equal(out[3L], 2016)      # **逐人**，不是同一个值
  expect_true(is.na(out[4L]))      # < 1900 视为脏值
})

test_that("不给 ongoing_fill 时 -313 变 NA（而不是留着算出负时长）", {
  expect_true(is.na(ukbcareer:::.clean_year(-313)))
})

test_that("工时档映射到中点", {
  expect_equal(ukbcareer:::.hours_from_cat(c(-1520, -2030, -3040, 4000)),
               c(17.5, 25, 35, 48))
  expect_true(is.na(ukbcareer:::.hours_from_cat(-99)))
})

test_that("SOC 层级由整除得到，不需要外部编码表", {
  expect_equal(ukbcareer:::.soc_level(2211, "soc_l4"), 2211)
  expect_equal(ukbcareer:::.soc_level(2211, "soc_l3"), 221)
  expect_equal(ukbcareer:::.soc_level(2211, "soc_l2"), 22)
  expect_equal(ukbcareer:::.soc_level(2211, "soc_l1"), 2)
  expect_error(ukbcareer:::.soc_level(2211, "soc_l9"), "Unknown SOC level")
})

test_that("SOC_DIVISOR 的键是字符串（整数键会埋静默 bug）", {
  expect_type(names(SOC_DIVISOR), "character")
  expect_equal(SOC_DIVISOR[["soc_l4"]], 1L)
})

test_that("性别编码归一认 0/1 与 Female/Male", {
  expect_equal(as.character(ukbcareer:::.norm_sex(c(0, 1))), c("Female", "Male"))
  expect_equal(as.character(ukbcareer:::.norm_sex(c("F", "male"))),
               c("Female", "Male"))
  expect_error(ukbcareer:::.norm_sex(c(0, 7)), "unparseable sex")
})

test_that("intens 的 13 维布局与暴露顺序是契约", {
  expect_equal(N_INTENS, 10L + 3L)
  expect_equal(length(EXPOSURES), N_EXPOSURE)
  expect_equal(IDX_DK, N_INTENS - 1L)     # 0-based
  expect_setequal(names(EXPOSURE_FIELDS), EXPOSURES)
})

test_that("向前填充（LOCF）不跨越起点", {
  expect_equal(ukbcareer:::.fill_down(c(NA, 1, NA, 2, NA)),
               c(NA, 1, 1, 2, 2))
})
