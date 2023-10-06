test_that("plots build properly", {
  library(dplyr)

  expect_no_error({
    report_start <- as.Date("2021-01-01")
    report_end <- as.Date("2021-02-01")

    disease_dash(type = "cases",
                 region = "california",
                 start = report_start,
                 end = report_end,
                 load_cache = F) %>%
      add_plot(outcome = "cases")
  })
})

test_that("tables build properly", {
  library(dplyr)

  expect_no_error({
    report_start <- as.Date("2021-01-01")
    report_end <- as.Date("2021-02-01")

    disease_dash(type = "cases",
                 region = "california",
                 start = report_start,
                 end = report_end,
                 load_cache = F) %>%
      add_table("deaths")
  })
})

test_that("plot input checking", {
  report_start <- as.Date("2021-01-01")
  report_end <- as.Date("2021-02-01")

  report <- disease_dash(type = "cases",
                         region = "california",
                         start = report_start,
                         end = report_end,
                         load_cache = F)

  expect_error(add_plot("not a report", "cases"))

  expect_error(add_plot(report, "error"))

  expect_error(add_plot(report, "cases", c("a", "b", "c", "d")))
})

test_that("table input checking", {
  report_start <- as.Date("2021-01-01")
  report_end <- as.Date("2021-02-01")

  report <- disease_dash(type = "cases",
                         region = "california",
                         start = report_start,
                         end = report_end,
                         load_cache = F)

  expect_error(add_table("not a report", "cases"))
  expect_error(add_table(report, "nothing"))
  expect_error(add_table(report, "cases", order = "error"))
})
