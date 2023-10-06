
# DiseaseDash

<!-- badges: start -->
<!-- badges: end -->

DiseaseDash allows you to quickly build a informative report on COVID-19 using data from the NYT.

## Installation

You can install DiseaseDash from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tchervo/DiseaseDash")
```

## Example - Build a report

DiseaseDash allows users to build a report using a handful of simple functions:

- `disease_dash()`: This function creates the base of the report that the other functions build upon. You specify the region and time period for your report here. The underlying data in the report can be modified using the `modify_data()` function

- `add_plot()`: This function adds a map showing the change in cases or deaths in the region specified previously. The colors used in the gradient are customizable in this function and you can modify the whole plot using the `modify_plot()` function

- `add_table()`: This adds a table showing the change in cases or deaths in the region specified previously for a selected number of sub-regions of the region specified previously. The number of sub-regions shown can be customized using the `rows` option

- `build_report()`: This function compiles all the above components into a single report that can be saved using `ggsave()` from the **{ggplot2}** package.

Let's take a look at COVID-19 cases in California between 06-01-2021 and 07-01-2021

``` r
library(DiseaseDash)
library(dplyr)

disease_dash(type = "cases",
             region = "california",
             start = as.Date("2021-06-01"),
             end = as.Date("2021-07-01")) %>%
add_plot("deaths") %>%
add_table("cases") %>%
build_report()

```

## Support

All functions have documentation that can be accessed within RStudio by using `?[function name]` or `help("function name")` within the RStudio console (Ex: `?disease_dash`)
