library(tidyverse)
library(lubridate)

load("Rdata/homesales.Rdata")

setline <- tibble(
    listingdate = seq(today() - days(100), today(), by = "day"),
    timeonmarket = today() - listingdate
)

med2022 <-
    homesales %>%
    filter(listingyear == 2022, !is.na(saledate)) %>%
    pull(timeonmarket) %>%
    max(.) %>%
    as.numeric(.)


homesales %>%
    filter(listingyear >= 2022) %>%
    mutate(onmarket = case_when(
        is.na(saledate) & is.na(undercontract) ~ "for sale",
        is.na(saledate) ~ "under contract",
        TRUE ~ "sold"
    )) %>%
    mutate(timeonmarket = as.numeric(timeonmarket)) %>%
    select(listingdate, onmarket, timeonmarket) %>%
    ggplot() +
    aes(x = listingdate, y = timeonmarket, color = onmarket) +
    geom_point() +
    geom_line(data = setline, aes(color = NULL), color = "black", alpha = .3) +
    geom_hline(yintercept = med2022, alpha = .3)