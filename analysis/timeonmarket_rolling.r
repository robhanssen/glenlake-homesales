library(tidyverse)
library(zoo)
theme_set(theme_light())

load("Rdata/homesales.Rdata")

window_size <- 19

homesales %>%
    filter(!is.na(saledate)) %>%
    arrange(listingdate) %>%
    mutate(
        time_med = zoo::rollmedian(timeonmarket,
            window_size,
            na.pad = TRUE,
            align = "right"
        ),
        time_av = zoo::rollmean(timeonmarket,
            window_size,
            na.pad = TRUE,
            align = "right"
        )
    ) %>%
    filter(listingdate > as.Date("2018-01-01")) %>%
    ggplot() +
    aes(x = listingdate, y = time_med) +
    geom_line() +
    geom_line(aes(y = time_av), lty = 2) +
    scale_y_continuous(limits = c(0, NA), breaks = seq(0, 500, 50)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    geom_point(aes(y = timeonmarket), alpha = .3, color = "gray50") + 
    labs(x = "Date of listing",
         y = "Time on market (rolling mean or median; in days)")


homesales %>%
    filter(!is.na(saledate)) %>%
    arrange(listingdate) %>%
    filter(listingdate > as.Date("2018-01-01")) %>%
    ggplot() +
    aes(x = listingyear, y = timeonmarket, group = listingyear) +
    geom_boxplot() +
    # geom_line(aes(y = time_av), lty = 2) +
    scale_y_continuous(limits = c(0, NA), breaks = seq(0, 500, 50)) +
    # scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    geom_jitter(aes(y = timeonmarket), alpha = .3, color = "gray50", width = .2) + 
    labs(x = "Date of sale",
         y = "Time on market (rolling mean or median; in days)")


