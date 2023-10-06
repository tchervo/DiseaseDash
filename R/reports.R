#' Initialize a DiseaseDash report
#'
#' @param type The type of report to make. Either "cases" or "vax"
#' @param region The region you want to show the data for. Either a U.S state or "national" for the whole U.S
#' @param load_cache Whether or not to load data from the cache. Recommended if you are running multiple reports.
#'     Default is TRUE.
#' @param save_cache Whether or not to save data from the cache if you are downloading from a data source.
#'  Default is TRUE.
#' @param start The start date of the time interval to include in the report. Should be a date object
#' @param end The end date that should be included in the report. Should be a date object
#' @param include_bounds Optional. Whether start and end should be included in the dates. Default is TRUE
#' @param data_url Optional. An alternate URL pointing to the NYT COVID-19 data
#'
#' @return A DiseaseDash report object that can be piped through to other report functions
#' @export
#'
#' @examples
#' \dontrun{
#'  library(dplyr)
#'
#'   my_report <- report(type = "cases",
#'                       region = "california",
#'                       start = as.Date("2021-11-01"),
#'                       end = as.Date("2022-01-01")) %>%
#'               add_plot("cases")
#' }
disease_dash <- function(type, region, start, end, include_bounds = T,
                         load_cache = T, save_cache = T, data_url = NULL) {

  stopifnot(type %in% c("cases", "vax"))
  stopifnot(class(start) == "Date")
  stopifnot(class(end) == "Date")
  stopifnot(start < end)

  # Check if region is valid
  region = tolower(region)
  valid_regions = ggplot2::map_data("county")$region

  # Initialize as FALSE so we only need to update once
  did_load_cache = F

  if (!(region %in% valid_regions | region == "national")) {
    stop(paste(region, "is not a valid region! Please enter a U.S state or 'national' for all states"))
  }

  # Cache logic:
  # If loading from the cache, check to see if a file exists and if not,
  # download the requested data type

  # Afterwards, if we should save to the cache and downloaded new data, save it
  # if we loaded from the cache, skip saving
  if (load_cache) {
    if (check_cache(type)) {
      report_dat <- read_cache(type)
      did_load_cache = T
    } else {
      print(paste("No cache data for", type, "found! Pulling from data sources..."))

      if (type == "cases") {
        report_dat <- get_infection_data(data_url = data_url)
      } else {
        report_dat <- get_vax_data()
      }
    }
  } else {
    if (type == "cases") {
      report_dat <- get_infection_data()
    } else {
      report_dat <- get_vax_data()
    }
  }


  if (save_cache & !did_load_cache) write_cache(report_dat, type)

  if (include_bounds) {
    report_dat <- report_dat %>%
      dplyr::filter(date >= start,
                    date <= end)
  } else {
    report_dat <- report_dat %>%
      dplyr::filter(date > start,
                    date < end)
  }

  if (region != "national") {
    report_dat <- dplyr::filter(report_dat, state == {{region}})
  }

  if (type == "cases") {
    report_dat <- report_dat %>%
      dplyr::group_by(state, county) %>%
      dplyr::mutate(tot_change_cases = sum(change_cases, na.rm = T),
                    tot_change_deaths = sum(change_deaths, na.rm = T)) %>%
      dplyr::slice_max(date) %>%
      dplyr::ungroup()
  } else {
    # TODO: Implement vax data
  }

  obj <- list(type = type, data = report_dat, region = region,
              start_date = start, end_date = end)
  class(obj) <- "DiseaseDashReport"

  obj
}

#' Compile a DiseaseDash report
#'
#' @param report The report to be built
#' @param new_title Optional. A new title to be displayed for the report. Default is "DiseaseDash report for region"
#'
#' @return A `grob` from the **{grid}** package that can be saved using ggplot2::ggsave()
#' @export
#'
#' @examples
#' \dontrun{
#'   library(ggplot2)
#'   library(dplyr)
#'
#'   my_report <- report(type = "cases",
#'                       region = "california",
#'                       start = as.Date("2021-11-01"),
#'                       end = as.Date("2022-01-01")) %>%
#'               add_plot("cases") %>%
#'               add_table("death") %>%
#'               build_report()
#'
#'  ggsave("my_report.pdf", my_report)
#' }
build_report <- function(report, new_title = NULL) {
  stopifnot(class(report) == "DiseaseDashReport")

  has_plot <- !is.null(report[["plot"]])
  has_table <- !is.null(report[["table"]])

  reg <- ifelse(report$region == "national",
                "United States",
                stringr::str_to_sentence(report$region))

  if (!has_plot & !has_table) stop("Your report must have a plot or a table to be built!")

  if (has_table) tab <- flextable::as_raster(report$table)

  if (!is.null(new_title)) {
    title <- new_title
  } else {
    title <- paste("DiseaseDash report for", reg)
  }

  subtitle <- paste(format(report$start_date, "%m-%d-%Y"),
                    "through",
                    format(report$end_date, "%m-%d-%Y"))

  top_lab <- paste0(title, "\n", subtitle)

  if (has_plot & has_table) {
    out <- gridExtra::grid.arrange(report$plot, grid::rasterGrob(tab), top = top_lab)
  } else if(has_plot & !has_table) {
    out <- gridExtra::grid.arrange(report$plot, top = top_lab)
  } else {
    out <- gridExtra::grid.arrange(grid::rasterGrob(tab), top = top_lab)
  }

  out
}
