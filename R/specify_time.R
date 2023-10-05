#' Specify the time intervals for a DiseaseDash report
#'
#' @param report The report you are specifying the time intervals for
#' @param start The start date of the time interval to include in the report
#' @param end The end date that should be included in the report
#' @param include_bounds Whether start and end should be included in the dates. Default is TRUE
#'
#' @return A DiseaseDash report object with the time interval specified
#' @export
#'
#' @examples
specify_time <- function(report, start, end, include_bounds = T) {
  stopifnot(class(report) == "DiseaseDashReport")
  stopifnot(class(start) == "Date")
  stopifnot(class(end) == "Date")

  if (include_bounds) {
    report$data <- report$data |>
      dplyr::filter(date >= start,
             date <= end)
  } else {
    report$data <- report$data |>
      dplyr::filter(date > start,
             date < end)
  }

  report$start_date <- start
  report$end_date <- end

  report
}
