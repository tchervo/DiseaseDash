build_geo_plot <- function(.data, geo, outcome) {
  geo = tolower(geo)
  geo_title = stringr::str_to_sentence(geo)

  if (geo == "national") {
    map_dat = ggplot2::map_data("county")
  } else {
    map_dat = ggplot2::map_data("county", geo)
  }

  map_dat = map_dat |>
    dplyr::rename(state = region, county = subregion)

  plot_dat = dplyr::inner_join(.data, map_dat, by = c("state", "county"))

  ggplot2::ggplot(plot_dat,
                  ggplot2::aes(x = long, y = lat, group = group,
                               fill = change_cases)) +
    ggplot2::geom_polygon(color = "black") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = paste("Change in", outcome))) +
    ggplot2::labs(title = paste("COVID-19", outcome, "in", geo_title))
}
