#' Pull COVID-19 infection data
#'
#' @description
#' This function downloads COVID-19 data from the NYT off of their Github repo
#' located at: https://github.com/nytimes/covid-19-data. In case the location has
#' changed, you can specify an updated URL using the data_url parameter. Most users
#' won't need this function, but it is here if you want to look at the data yourself
#'
#'
#' @param cache Should the data be cached to speed up times for making multiple reports? Default is true.
#'   See details for more info
#' @param data_url Optional. An alternate URL pointing to the NYT COVID-19 data
#'
#' @return A data.frame of the latest NYT county-level COVID-19 data
#' @export
#'
#' @examples
#' covid_dat = get_infection_data()
get_infection_data <- function(cache = T, data_url = NULL) {
  if (is.null(data_url)) {
    dat = read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  } else {
    dat = read.csv(data_url)
  }

  dat |>
    dplyr::arrange(state, county, date) |>
    dplyr::group_by(county, state) |>
    dplyr::mutate(change_cases = cases - dplyr::lag(cases, default = 0),
                  change_deaths = deaths - dplyr::lag(deaths, default = 0),
                  state = tolower(state),
                  county = tolower(county)) |>
    dplyr::ungroup()
}

# TODO: Find data source and implement
get_vax_data <- function(data_url = NULL) {

}
