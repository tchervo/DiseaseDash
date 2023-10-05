#' Initialize a DiseaseDash report
#'
#' @param type The type of report to make. Either "cases" or "vax"
#' @param region The region you want to show the data for. Either a U.S state or "national" for the whole U.S
#' @param load_cache Whether or not to load data from the cache. Recommended if you are running multiple reports.
#'     Default is TRUE.
#' @param save_cache Whether or not to save data from the cache if you are downloading from a data source.
#'  Default is TRUE.
#'
#' @return A DiseaseDash report object that can be piped through to other report functions
#' @export
#'
#' @examples
disease_dash <- function(type, region, load_cache = T, save_cache = T) {
  stopifnot(type %in% c("cases", "vax"))

  # Check if region is valid
  region = tolower(region)
  valid_regions = ggplot2::map_data("county")$region

  if (!(region %in% valid_regions | region == "national")) {
    stop(paste(region, "is not a valid region! Please enter a U.S state or 'national' for all states"))
  }

  if (load_cache) {
    if (check_cache(type)) {
      report_dat <- read_cache(type)
    } else {
      print(paste("No cache data for", type, "found! Pulling from data sources..."))

      if (type == "cases") {
        report_dat <- get_infection_data()
      } else {
        report_dat <- get_vax_data()
      }

      write_cache(report_dat, type)
    }
  } else {
    if (type == "cases") {
      report_dat <- get_infection_data()
    } else {
      report_dat <- get_vax_data()
    }

    write_cache(report_dat, type)
  }

  obj <- list(type = type, data = report_dat, region = region)
  class(obj) <- "DiseaseDashReport"

  obj
}
