library(tidyverse)
library(lubridate)
library(doParallel)
library(ggridges)
source("functions/config.r")
theme_set(theme_light())

doParallel::registerDoParallel(cores = 8)

load("Rdata/homesales.Rdata")


period_list <- sort(c(60, 120, 90 * 2:6))

multiperiod <- function(period) {

    library(tidyverse)
    library(lubridate)
    library(doParallel)

    mc <- function(value, change = 0, variability = .2) {
        ch <- rnorm(1, change, variability)
        outval <- value * (1 + ch)
        round(outval, 0)
    }


    doParallel::registerDoParallel(cores = 8)
    jnow <- today()
    homesales_adjusted <- homesales %>% filter(!is.na(amount))

    cumulativelist <- homesales %>%
        arrange(listingdate) %>%
        mutate(y = 1, culisting = cumsum(y))

    cusales <- homesales_adjusted %>%
    arrange(saledate) %>%
    mutate(y = 1, cusale = cumsum(y), cumkt = cumsum(amount))


    mod_listings <- lm(culisting ~ listingdate, data = cumulativelist %>%
        filter(listingdate > jnow - days(period))) %>%
        coef(.)
    slope <- as.numeric(mod_listings[2])

    freq <- 1 / slope

    median_sale <- with(
        homesales_adjusted %>% filter(saledate > jnow - days(period)),
        median(amount, na.rm = TRUE)
    )

    median_time <- with(
        homesales_adjusted %>% filter(saledate > jnow - days(period)),
        median(timeonmarket)
    )


    mkt_beginyear <- cusales %>%
        filter(saledate >= floor_date(jnow, unit = "year")) %>%
        summarize(min = min(cumkt), max = max(cumkt)) %>%
        mutate(growth = max - min)

    n_max <- 1000
    now <- today()
    year_end <- ceiling_date(now, unit = "year")

    simul <- function(n) {
        totalsale <- 0
        ldate = now
        # year_end <- lubridate::ceiling_date(now, unit = "year")

        while (ldate < year_end) {
            ldate <- ldate + lubridate::days(mc(freq, variability = .5))
            sdate <- ldate + lubridate::days(mc(median_time, variability = .5))
            if (sdate < year_end) {
                sale <- mc(median_sale, variability = .2)
                totalsale <- totalsale + sale
            }
        }
        totalsale + mkt_beginyear$growth
    }

    numberlist <- foreach(i = 1:n_max, .combine = rbind) %dopar% simul(i)
    alist <- as_tibble(numberlist) %>% mutate(period = period)

}

multip <- foreach(p =period_list, .combine = rbind) %dopar% multiperiod(p) %>%
    rename(data = V1)# %>% 
    #mutate(data = data + mkt_beginyear$growth)

ggplot(multip) + 
    aes(data, factor(period), fill = factor(period)) + 
    geom_density_ridges(alpha = .2) +
    scale_x_continuous(labels = scales::dollar_format())

ggsave("montecarlo/mc-par.png")