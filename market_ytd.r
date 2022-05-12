library(tidyverse)
library(lubridate)
theme_set(theme_light())

load("Rdata/homesales.Rdata")

now <- yday(today())

homesales_ytd <-
    homesales %>%
    mutate(doy_sale = yday(saledate),
           doy_listing = yday(listingdate))

inner_join(
    homesales_ytd %>%
    group_by(listingyear) %>%
    filter(doy_listing <= now) %>%
    count(listingyear) %>%
    rename(year = listingyear, listings = n),

    homesales_ytd %>%
    group_by(saleyear) %>%
    filter(doy_sale <= now) %>%
    count(saleyear) %>%
    rename(year = saleyear, sales = n)
) %>% 
    pivot_longer(c(sales,listings), names_to = "type", values_to = "count") %>%
    ggplot +
    aes(x = type, y = count, fill = factor(year)) + 
    geom_col(position = "dodge") + 
    scale_fill_viridis_d()