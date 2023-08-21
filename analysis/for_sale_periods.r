library(tidyverse)
library(lubridate)
library(scales)
theme_set(theme_light())
source("functions/config.r")
load("Rdata/homesales.Rdata")

glenlakehomes <- read_csv("sources/glenlakehomes.csv")
hometypes <- read_csv("sources/hometypes.csv")

totalhomes <- with(glenlakehomes, sum(numberofhomes))
totalsold <- nrow(homesales)
averageturnover <- totalsold / totalhomes

pdf_lognorm <- function(x, mu, sig) {
       .5 * (1 + pracma::erf((log(x) - mu) / (sqrt(2) * sig)))
}

homesales %>%
    drop_na(saledate) %>%
    ggplot(aes(y = fct_reorder(address, desc(saledate)))) +
    geom_segment(aes(x = saledate, xend = listingdate, yend = address), color = "black")


resold_address <-
       homesales %>%
       count(address, sort = TRUE) %>%
       filter(n > 1)

homesales %>%
       semi_join(resold_address) %>%
       ggplot(aes(y = fct_reorder(address, desc(listingdate)))) +
       geom_segment(aes(x = listingdate, xend = saledate, yend = address), color = "black")
