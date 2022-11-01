library(tidyverse)
library(lubridate)
theme_set(theme_minimal())

load("Rdata/homesales.Rdata")


sale_turnover <-
    homesales %>%
    filter(!is.na(saledate)) %>%
    arrange(saledate) %>%
    mutate(y = 1, 
          sold = cumsum(y), 
          turnover = sold / 482
    ) %>%
    rename(date = saledate)

listing_turnover <-
    homesales %>%
    # filter(!is.na(saledate)) %>%
    arrange(listingdate) %>%
    mutate(y = 1, 
          listed = cumsum(y), 
          turnover = listed / 482
    ) %>%
    rename(date = listingdate)

ggplot(listing_turnover) + 
    aes(x = date, y = turnover) + 
    geom_line(show.legend = FALSE, color = "dodgerblue") +
    geom_line(data = sale_turnover, color = "red") +
    scale_y_continuous(labels = scales::percent_format())