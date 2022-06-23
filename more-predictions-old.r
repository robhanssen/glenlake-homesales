library(tidyverse)
library(lubridate)
source("functions/config.r")
source("functions/horizon-predictions.r")
theme_set(theme_light())

load("Rdata/homesales.Rdata")

period_list <- sort(c(60, 120, 90 * 2:4))

homesales_adjusted <- homesales %>% filter(!is.na(amount))

p1 <-
    map_df(period_list, ~ predict_market_size(homesales_adjusted, .x)) %>%
    ggplot() +
    aes(factor(period), amount_sold, fill = hometype) +
    geom_col() +
    scale_y_continuous(
        labels = scales::dollar_format(),
        breaks = 2e6 * 0:10
    ) +
    labs(
        x = "# of days used in prediction",
        y = "Predicted market size ($)",
        fill = "Hometype",
        title = paste("Market size prediction for end of year", year(today()))
    ) +
    theme(legend.position = "none")

ggsave("predictions/horizon-market-value.png", plot = p1)

#
# listing rate
#

p2 <-
    map_df(period_list, ~ predict_listing_rate(homesales, .x)) %>%
    ggplot() +
    aes(factor(period), amount_listed, fill = hometype) +
    geom_col() +
    scale_y_continuous(
        labels = scales::comma_format(),
        breaks = 10 * 0:10
    ) +
    labs(
        x = "# of days used in prediction",
        y = "Predicted number of listings",
        fill = "Hometype",
        title = paste("Listing rate prediction for end of year", year(today()))
    ) +
    theme(legend.position = "none")

ggsave("predictions/horizon-listings-rate.png", plot = p2)

#
# sale rate
#

p3 <-
    map_df(period_list, ~ predict_sale_rate(homesales, .x)) %>%
    ggplot() +
    aes(factor(period), amount_listed, fill = hometype) +
    geom_col() +
    scale_y_continuous(
        labels = scales::comma_format(),
        breaks = 10 * 0:10
    ) +
    labs(
        x = "# of days used in prediction",
        y = "Predicted number of sales",
        fill = "Hometype",
        title = paste("Sale rate prediction for end of year", year(today()))
    ) +
    theme(legend.position = "none")

ggsave("predictions/horizon-sale-rate.png", plot = p3)