test_that("base report data is correct", {
  report_start <- as.Date("2021-01-01")
  report_end <- as.Date("2021-02-01")

  report <- disease_dash(type = "cases",
                         region = "california",
                         start = report_start,
                         end = report_end,
                         load_cache = F)

  # Make sure the only region in the data is the one specified
  expect_equal(unique(report$data$state), "california")
  # Make sure the dates are filtered right
  # Only the last date is kept in the data so we only check the max
  expect_equal(max(report$data$date), report_end)
})

test_that("reports build properly", {
  library(dplyr)

  expect_no_error({
    report_start <- as.Date("2021-01-01")
    report_end <- as.Date("2021-02-01")

   disease_dash(type = "cases",
                region = "california",
                start = report_start,
                end = report_end,
                load_cache = F) %>%
     add_plot("cases") %>%
     add_table("deaths") %>%
     build_report()
})
})

test_that("data checking for report building", {
  expect_error(build_report("not a report"))
})
