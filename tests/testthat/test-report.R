## 个体报告：能画、能存、边界情形不崩

skip_if_not_installed("ggplot2")
skip_if_not_installed("patchwork")

d <- ukb_synth(n = 60)
wl <- ukb_worklife(d, verbose = FALSE)

test_that("the synthetic showcase person is the last row and looks as designed", {
  id <- attr(d, "showcase_eid")
  expect_equal(d$eid[nrow(d)], id)
  co <- wl$cohort[eid == id]
  expect_equal(co$career_end, 59)
  expect_equal(co$career_end_source, "retire")
  a <- wl$annual[eid == id]
  expect_equal(a[age == 35]$is_transition, 1L)
  expect_equal(a[age == 44]$is_parallel, 1L)
})

test_that("plot_person() builds for the showcase and for every edge case", {
  for (id in c(attr(d, "showcase_eid"), d$eid[1:13])) {
    if (!nrow(wl$annual[eid == id])) next      # #11：唯一的 job 被剔除
    p <- plot_person(wl, id)
    expect_s3_class(p, "patchwork")
    expect_no_error(ggplot2::ggplot_build(p[[1]]))
    expect_no_error(ggplot2::ggplot_build(p[[2]]))
  }
})

test_that("plot_person() accepts a ukb_career() result and gives clear errors", {
  res <- ukb_career(wl, verbose = FALSE)
  expect_s3_class(plot_person(res, attr(d, "showcase_eid")), "patchwork")
  expect_error(plot_person(wl, 1L), "not in the data")
  expect_error(plot_person(wl, d$eid[1:2]), "single person")
})

test_that("ukb_report() writes a multi-page PDF and skips bad eids", {
  f <- tempfile(fileext = ".pdf")
  expect_warning(ukb_report(wl, c(d$eid[1:3], 1L), f), "1 of 4 eids skipped")
  expect_gt(file.size(f), 1000)
  expect_error(ukb_report(wl, d$eid[1:2], tempfile(fileext = ".png")), "one person")
})
