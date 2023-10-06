#' @importFrom dplyr mutate rename filter group_by arrange lag inner_join ungroup
#'     %>%
#' @importFrom ggplot2 ggplot aes geom_polygon theme_classic theme map_data
#'     theme guides labs guide_legend element_text element_blank
#' @importFrom stringr str_to_sentence
#' @importFrom utils read.csv head
#' @importFrom purrr map_dfr map
#' @importFrom grid textGrob rasterGrob
#' @importFrom gridExtra grid.arrange
#' @importFrom flextable flextable set_header_df as_flextable as_raster
"_PACKAGE"

utils::globalVariables(c("region", "subregion", "long", "lat",
                         "group", "change_cases", "change_deaths",
                         "state", "county", "cases", "deaths", "target",
                         "new_text"))
