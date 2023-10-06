#' Add a plot to a DiseaseDash report
#'
#' @description
#' This function generates a map filled with a color gradient showing the change
#' in COVID-19 cases or deaths during the time interval specified in the report.
#' Currently, vaccination data isn't supported
#'
#' Extensive customizations to this plot can be made using the `modify_plot` function
#'
#'
#' @param report The DiseaseDash report to add a plot to
#' @param outcome The outcome to be displayed in the plot. Either "cases" or "deaths"
#' @param colors The colors corresponding to the low, medium, and high points on the plot
#'
#' @return The DiseaseDash report object with a plot added
#' @export
#'
#' @examples
#' \dontrun{
#'   my_report <- disease_dash(type = "cases",
#'                            region = "california",
#'                            start =  as.Date("2021-04-01"),
#'                            end = as.Date("2021-05-01")) %>%
#'               add_plot("cases")
#' }
add_plot <- function(report, outcome,
                     colors = c(low = "#1aab4a", mid = "#e02f0b", high = "#d837ed")) {

  # Check if the names in the argument to colors match what they should be
  if (sum(names(colors) %in% c("low", "mid", "high")) != 3) stop("Arugment to `colors` must be named low, mid, and high!")
  stopifnot(length(colors) == 3)

  stopifnot(class(report) == "DiseaseDashReport")
  # TODO: Implement support for vax
  stopifnot(outcome %in% c("cases", "deaths"))

  region = report$region
  region_title = stringr::str_to_sentence(region)

  if (region == "national") {
    map_dat = ggplot2::map_data("county") %>%
      rename(state = region, county = subregion)
  } else {
    # This allows the users to subset a state
    # down to certain counties to only display a specific area
    # using the modify functions
    map_dat = ggplot2::map_data("county", region) %>%
      rename(state = region, county = subregion) %>%
      filter(county %in% report$data$county)
  }

  plot_dat <- dplyr::left_join(map_dat, report$data, by = c("state", "county")) %>%
    dplyr::rename_with(~dplyr::case_when(.x == "tot_change_cases" & {{outcome}} == "cases" ~ "target",
                                         .x == "tot_change_deaths" & {{outcome}} == "deaths" ~ "target",
                                         T ~ .x))

  plot <- ggplot2::ggplot(plot_dat,
                  ggplot2::aes(x = long, y = lat, group = group,
                               fill = target)) +
    ggplot2::geom_polygon(color = "black") +
    ggplot2::scale_fill_gradient2(low = colors[["low"]],
                                  mid = colors[["mid"]],
                                  high = colors[["high"]],
                                  n.breaks = 5,
                                  limits = c(0, NA)) +
    ggplot2::theme_void() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(title = paste("Change in", outcome)))

  report$plot <- plot

  report
}

add_table <- function(report, outcome, rows = 5, order = "desc") {
  out_col <- ifelse(outcome == "cases",
                    "tot_change_cases",
                    "tot_change_deaths")

  out_header <- ifelse(outcome == "cases",
                       "Change in COVID-19 cases",
                       "Change in COVID-19 deaths")

  # Styled region
  reg <- ifelse(report$region == "national",
                "United States",
                stringr::str_to_sentence(report$region))

  # Symbol for dplyr calls
  out_sym <- as.symbol(out_col)

  # Rename to target makes it easier to specify the outcome in some functions
  table_dat <- report$data %>%
    dplyr::rename_with(~dplyr::if_else(.x == {{out_col}}, "target", .x))

  # Wraps the variable passed in desc() from dplyr if the user specifies descending
  .int_order <- function(x) if (order == "desc") dplyr::desc(x) else x

  if(report$region == "national") {
    ref <- data.frame(key = c("state", "target"),
                      label = c(state = "State", target = out_header))

    tab <- table_dat %>%
      dplyr::arrange(.int_order(target)) %>%
      dplyr::select(state, target) %>%
      dplyr::mutate(state = stringr::str_to_sentence(state)) %>%
      head(rows) %>%
      flextable::as_flextable() %>%
      flextable::set_header_df(mapping = ref)
  } else {
    ref <- data.frame(key = c("state", "county", "target"),
                      label = c(state = "State",
                                county = "County",
                                target = out_header))

    tab <- table_dat %>%
      dplyr::arrange(state, .int_order(target)) %>%
      dplyr::select(state, county, target) %>%
      dplyr::mutate(state = stringr::str_to_sentence(state),
                    county = stringr::str_to_sentence(county)) %>%
      head(rows) %>%
      flextable::as_flextable() %>%
      flextable::set_header_labels(values = list(state = "State",
                                                 county = "County",
                                                 target = out_header)) %>%
      flextable::set_caption(caption = paste("Top", rows, "for", outcome, "in",
                                             reg))
  }

  report$table <- tab

  report
}
