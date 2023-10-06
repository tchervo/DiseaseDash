#' Modify the data in a DiseaseDash report
#'
#' @param report The report you want to modify
#' @param fn The function you would like to apply to the data.
#' @param ... Additional arguments for `fn` if needed
#'
#' @return The report passed to this function with modified data
#' @export
#'
#' @examples
#' \dontrun{
#'   library(dplyr)
#'
#'   my_report <- report(type = "cases",
#'                       region = "california",
#'                       start = as.Date("2021-11-01"),
#'                       end = as.Date("2022-01-01")) %>%
#'               modify_data(~filter(.x, county %in% c("alameda", "san francisco")))
#' }
modify_data <- function(report, fn, ...) {
  stopifnot(class(report) == "DiseaseDashReport")

  report$data <- list(report$data) %>%
    purrr::map_dfr(fn, ...)

  report
}

#' Modify the plot in a DiseaseDash report
#'
#' @param report The report you want to modify
#' @param fn The function you would like to apply to the plot.
#' @param ... Additional arguments for `fn` if needed
#'
#' @return The report passed to this data with a modified plot
#' @export
#'
#' @examples
#' \dontrun{
#'   library(dplyr)
#'
#'   my_report <- report(type = "cases",
#'                       region = "california",
#'                       start = as.Date("2021-11-01"),
#'                       end = as.Date("2022-01-01")) %>%
#'               add_plot("cases") %>%
#'               modify_plot(function (x) x + ggplot2::labs(title = "Cases in California"))
#' }
modify_plot <- function(report, fn, ...) {
  if (is.null(report$plot)) stop("This report does not have a plot!")

  args <- list(x = report$plot, ...)

  report$plot <- do.call(fn, args)

  report
}
