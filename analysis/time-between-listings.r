library(tidyverse)
library(lubridate)
theme_set(theme_light())

load("Rdata/homesales.Rdata")


listing_split <-
    homesales %>%
    arrange(listingdate) %>%
    mutate(time_between_listings = lead(listingdate) - listingdate) %>%
    select(listingdate, time_between_listings, year = listingyear) 
    
listing_split %>%
    group_by(year) %>%
    summarise(mn_time = mean(time_between_listings, na.rm = TRUE),
             md_time = median(time_between_listings, na.rm = TRUE)
             )

listing_split %>%
    ggplot + aes(y = factor(year), x = time_between_listings) + 
    ggridges::geom_density_ridges()



sale_split <-
    homesales %>%
    filter(!is.na(saledate)) %>%
    arrange(saledate) %>%
    mutate(time_between_sales = lead(saledate) - saledate) %>%
    select(listingdate, time_between_sales, year = saleyear)
    
sale_split %>%
    group_by(year) %>%
    summarise(mn_time = mean(time_between_sales, na.rm = TRUE),
             md_time = median(time_between_sales, na.rm = TRUE)
             )

sale_split %>%
    ggplot + aes(y = factor(year), x = time_between_sales) + 
    ggridges::geom_density_ridges()
