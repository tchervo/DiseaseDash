#' @importFrom dplyr mutate rename filter group_by arrange lag inner_join ungroup
#' @importFrom ggplot2 ggplot aes geom_polygon theme_classic theme map_data
#'     theme guides labs guide_legend element_text element_blank
#' @importFrom stringr str_to_sentence
#' @importFrom utils read.csv
"_PACKAGE"

utils::globalVariables(c("region", "subregion", "long", "lat",
                         "group", "change_cases", "change_deaths",
                         "state", "county", "cases", "deaths"))
