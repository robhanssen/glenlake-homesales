library(tidyverse)
library(lubridate)
library(scales)
theme_set(theme_light())
source("functions/config.r")
load("Rdata/homesales.Rdata")

glenlakehomes <- read_csv("sources/glenlakehomes.csv")
hometypes <- read_csv("sources/hometypes.csv")


homesales %>%
       count(address) %>%
       filter(n > 1) %>%
       inner_join(homesales, by = "address", multiple = "all") %>%
       filter(!is.na(amount)) %>%
       arrange(address, saledate) %>%
       group_by(address) %>%
       mutate(
              price_inc = amount / lag(amount),
              delta_time = (saledate - lag(saledate)) / dyears(1),
              perc_inc = price_inc^(1 / delta_time) - 1
       ) %>% ungroup() %>% drop_na(perc_inc) %>%
       ggplot(aes(x = saledate, y = perc_inc)) +
       geom_point() +
       geom_smooth(method = "loess", se = TRUE, alpha = .4, color = "gray50", fill = "gray80") +
       scale_y_continuous(labels = scales::label_percent()) + 
       expand_limits(y = 0) + 
       labs(x = "", y = "Home price increase (CAGR%)")
