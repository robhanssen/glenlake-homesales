library(tidyverse)
source("functions/config.r")
load("Rdata/homesales.Rdata")

# unconfirmed sales for researching
homesales %>%
        filter(undercontract == 1 & !is.na(saledate)) %>%
        select(address:hometype, undercontract) %>%
        write_csv("data/unconfirmed_sales.csv")

# unsold homes
homesales %>%
        filter(is.na(saledate)) %>%
        select(address, listingdate, hometype, undercontract) %>%
        arrange(listingdate) %>%
        write_csv("data/unsold_homes.csv")

# homes on market
homesales %>%
        filter(is.na(saledate), is.na(undercontract)) %>%
        select(address, listingdate, hometype, undercontract) %>%
        arrange(listingdate) %>%
        write_csv("data/homes_on_market.csv")
