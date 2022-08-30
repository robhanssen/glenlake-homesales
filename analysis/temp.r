library(tidyverse)
library(lubridate)

load("Rdata/homesales.Rdata")

resales <-
    homesales %>%
    arrange(address, listingdate) %>%
    group_by(address) %>%
    mutate(lagtime = (listingdate - lag(saledate)) / dmonths(1)) %>%
    drop_na(lagtime) %>%
    mutate(address_year = glue::glue("{address} ({listingyear})"))

resales %>%
    ggplot() +
    aes(
        y = fct_reorder(address_year, -lagtime),
        x = lagtime,
        fill = hometype
    ) +
    geom_col() +
    labs(
        x = "Time between sale and next listing (in months)",
        y = "Address",
        fill = "Type of home", caption = caption
    ) +
    scale_x_continuous(breaks = 12 * 1:10) +
    theme(legend.position = c(.8, .8))