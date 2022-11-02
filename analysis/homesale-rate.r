library(tidyverse)
library(lubridate)

load("Rdata/homesales.Rdata")

min_date <- lubridate::ceiling_date(
    min(homesales$saledate, na.rm = TRUE),
    unit = "month"
)

max_date <- lubridate::ceiling_date(lubridate::today(),
    unit = "month"
)

date_range <- seq(min_date, max_date, "month")


period_length <- 3 # months

map_dfr(
    date_range,
    ~ tibble(
        date = .x,
        sold = homesales %>% filter(
            saledate < .x,
            saledate > .x - months(period_length)
        ) %>%
            nrow()
    )
) %>%
    ggplot() +
    aes(date, sold) +
    geom_point() +
    geom_line() +
    geom_smooth(method = "loess") +
    scale_y_continuous(breaks = 4 * 0:100) +
    theme_light()