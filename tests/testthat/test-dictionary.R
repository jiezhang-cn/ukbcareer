## 词典与 ukb_career() 的输出必须逐列对得上：新增派生列却忘了写词典，
## 用户就会拿到一列查不到含义的变量。

test_that("every column ukb_career() returns is in the dictionary", {
  out <- ukb_career(ukb_synth(n = 40), verbose = FALSE)
  dd <- ukb_dictionary(expand = TRUE)
  expect_setequal(setdiff(names(out), dd$variable), character(0))
})

test_that("every CSV-only dictionary entry is actually produced", {
  out <- ukb_career(ukb_synth(n = 40), verbose = FALSE)
  dd <- ukb_dictionary(what = c("core", "exposure"), expand = TRUE)
  expect_setequal(setdiff(dd$variable, names(out)), character(0))
})

test_that("lookup by name, by agent-specific name and by group works", {
  expect_equal(ukb_dictionary("career_end_source")$variable, "career_end_source")
  expect_equal(ukb_dictionary("yrs_high_asbestos")$what, "exposure")
  expect_true(all(ukb_dictionary(what = "movement")$what == "movement"))
  expect_equal(nrow(ukb_dictionary("z017")), 1L)
  expect_warning(ukb_dictionary("not_a_variable"), "Not a ukbcareer variable")
  expect_error(ukb_dictionary(what = "nope"), "Unknown")
  expect_output(print(ukb_dictionary()), "Career timing")
})

test_that("the default what does not warn when no bundle is given", {
  expect_no_warning(ukb_career(ukb_synth(n = 20), verbose = FALSE))
  expect_warning(ukb_career(ukb_synth(n = 20), what = "movement",
                            verbose = FALSE), "needs the model bundle")
})
