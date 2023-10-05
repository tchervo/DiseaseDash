#' Modify the data in a DiseaseDash report
#'
#' @param report The report you want to modify
#' @param fn The function you would like to apply to the data. Supports **{purrr}** style formulas
#' @param ... Additional arguments for `fn` if needed
#'
#' @return The report passed to this function with modified data
#' @export
#'
#' @examples
modify_data <- function(report, fn, ...) {
  report$data <- list(report$data) |>
    purrr::map_dfr(fn, ...)

  report
}

#' Modify the plot in a DiseaseDash report
#'
#' @param report The report you want to modify
#' @param fn The function you would like to apply to the plot. Supports **{purrr}** style formulas
#' @param ... Additional arguments for `fn` if needed
#'
#' @return The report passed to this data with a modified plot
#' @export
#'
#' @examples
modify_plot <- function(report, fn, ...) {

}
