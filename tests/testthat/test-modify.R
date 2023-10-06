test_that("data modification returns a data.frame", {
  library(dplyr)

  report_start <- as.Date("2021-01-01")
  report_end <- as.Date("2021-02-01")

  report <- disease_dash(type = "cases",
                         region = "california",
                         start = report_start,
                         end = report_end,
                         load_cache = F) %>%
    modify_data(~filter(.x, county %in% c("alameda")))

  expect_true("data.frame" %in% class(report$data))
})

test_that("plot modification returns a ggplot object", {
  library(dplyr)

  report_start <- as.Date("2021-01-01")
  report_end <- as.Date("2021-02-01")

  report <- disease_dash(type = "cases",
                         region = "california",
                         start = report_start,
                         end = report_end,
                         load_cache = F) %>%
    add_plot(outcome = "cases") %>%
    modify_plot(function (x) x + ggplot2::labs(title = "Hello, world!"))

  expect_true("ggplot" %in% class(report$plot))
})

test_that("plot modification input checking", {
  report_start <- as.Date("2021-01-01")
  report_end <- as.Date("2021-02-01")

  report <- disease_dash(type = "cases",
                         region = "california",
                         start = report_start,
                         end = report_end,
                         load_cache = F)

  expect_error(modify_plot("a", ~.x))
  # Make sure it check for a plot to be added
  expect_error(modify_plot(report, ~.x))
})

test_that("data modification error checking", {
  expect_error(modify_data("a", ~.x))
})
