library(tidyverse)
library(lubridate)
source("functions/config.r")
theme_set(theme_light())

load("Rdata/homesales.Rdata")

mc <- function(value, change = 0, variability = .2) {
    ch <- rnorm(1, change, variability)
    outval <- value * (1 + ch)
    round(outval, 0)
}

period_list <- sort(c(60, 120, 90 * 2:4))
period <- 180

homesalesorig <- homesales
jnow = ymd(20210630)

homesales <- homesales %>% filter(saledate < today() - years(1))


homesales_adjusted <- homesales %>% filter(!is.na(amount))

cumulativelist <- homesales %>%
    arrange(listingdate) %>%
    mutate(y = 1, culisting = cumsum(y))

cusales <- homesales_adjusted %>%
    arrange(saledate) %>%
    mutate(y = 1, cusale = cumsum(y), cumkt = cumsum(amount))

mod_listings <- lm(culisting ~ listingdate, data = cumulative %>% filter(listingdate > jnow - days(period))) %>% coef(.)
slope <- as.numeric(mod_listings[2])

exp_residual_sales <- as.numeric((ceiling_date(jnow, unit = "year") - jnow) * slope)
freq <- 1 / slope

median_sale <- with(
    homesales_adjusted %>% filter(saledate > jnow - days(period)),
    median(amount)
)

median_time <- with(
    homesales_adjusted %>% filter(saledate > jnow - days(period)),
    median(timeonmarket)
)


mkt_beginyear <- cusales %>%
    filter(saledate >= floor_date(jnow, unit = "year")) %>%
    summarize(min = min(cumkt), max = max(cumkt)) %>%
    mutate(growth = max - min)

n_max <- 200
numberlist <- rep(0, n_max)
now <- today()
year_end <- ceiling_date(now, unit = "year")

for (n in seq_along(numberlist)) {
    totalsale <- 0
    ldate <- now

    while (ldate < year_end) {
        ldate <- ldate + days(mc(freq))
        sdate <- ldate + days(mc(median_time))
        if (sdate < year_end) {
            sale <- mc(median_sale)
            totalsale <- totalsale + sale
        }
    }
    max_output <- totalsale
    # print(paste(n, totalsale))
    numberlist[n] <- max_output
}

hist(numberlist + mkt_beginyear$growth)
# boxplot(numberlist + mkt_beginyear$growth)