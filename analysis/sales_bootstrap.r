library(tidyverse)

load("Rdata/homesales.Rdata")

dat <- homesales %>%
    filter(hometype != "townhome" & !is.na(amount)) %>%
    nest(data = !saleyear) %>%
    mutate(count = map_int(data, nrow))


bootstrapping <- function(dat, n) {
    max <- nrow(dat)
    sample_list <- map(1:n, \(q) sample(1:max, max, replace = TRUE))
    map_dbl(
        sample_list, \(samples) with(dat[samples, ], median(amount))
    )
}


dat %>%
    mutate(
        m = map(data, bootstrapping, n = 1000),
        mn = map(m, \(l) quantile(l, prob = c(0.025, 0.5, 0.975)))
    ) %>%
    unnest_wider(mn) %>%
    ggplot(aes(x = factor(saleyear), y = `50%`, ymin = `2.5%`, ymax = `97.5%`)) +
    geom_errorbar(width = .3) +
    geom_point(size = 4, shape = 21)
