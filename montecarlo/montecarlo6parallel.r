library(tidyverse)
library(ggplot2)
library(ggridges)
theme_set(theme_light())


load("Rdata/homesales.Rdata")

period_list <- c(90, 120, 180, 360, 720)

# estimator of time between sales and listings modeled as exponential distribution
time_estimator <- function(dat, var, period, lambda = .1) {
    dat_adj <- dat %>%
        filter({{ var }} > today() - days(period)) %>%
        arrange({{ var }}) %>%
        pull({{ var }})
    diffs <- as.numeric(diff(dat_adj))

    e_cdf <- ecdf(diffs)
    est_tibble <- tibble(
        t = seq(0, max(diffs), 1),
        e = e_cdf(t)
    )

    mod <- nls(
        e ~ pexp(t, lambda0),
        start = list(lambda0 = lambda),
        data = est_tibble
    )

    coefficients(mod)
}

# estimator of sale amount modeled as lognormal distribution
price_estimator <- function(dat, period, mean0 = 12.7, sd0 = .2) {
    dat_adj <- dat %>%
        filter(saledate > today() - days(period)) %>%
        pull(amount)

    prices <- log(as.numeric(dat_adj))

    p_cdf <- ecdf(prices)

    est_tibble <- tibble(
        t = seq(min(prices), max(prices), .01),
        e = p_cdf(t)
    )

    mod <- nls(
        e ~ pnorm(t, mean0, sd0),
        start = list(mean0 = mean0, sd0 = sd0),
        data = est_tibble
    )

    coefficients(mod)
}

simul_sale <- function(dat, period, niter = 1000) {
    list_time_est <- time_estimator(dat, listingdate, period)
    sale_time_est <- time_estimator(dat, saledate, period)
    price_est <- price_estimator(dat, period = period)

    last_list <- max(dat$listingdate, na.rm = TRUE)
    last_sale <- max(dat$saledate, na.rm = TRUE)

    simul_sale_values <- map_dbl(seq_len(niter), \(n) {
        list_simul <- round(rexp(365, rate = list_time_est))
        sale_simul <- round(rexp(365, rate = sale_time_est))
        price_simul <- exp(rnorm(365, mean = price_est["mean0"], sd = price_est["sd0"]))

        next_listings <- last_list + cumsum(list_simul)
        next_sales <- sort(next_listings + sale_simul)

        est_sales_num <- length(next_sales[next_sales < ceiling_date(today(), unit = "year")])

        total_simul_sales <-
            sum(dat$amount[year(dat$saledate) == year(today())], na.rm = TRUE) +
            sum(price_simul[seq_len(est_sales_num)])

        total_simul_sales
    })

    simul_sale_values
}

multip <- map_df(period_list, \(p) {
    tibble(
        period = p,
        price = simul_sale(homesales, p)
    )
})

current_sales <- sum(homesales$amount[year(homesales$saledate) == year(today())], na.rm = TRUE)

ggplot(multip) +
    aes(x = price, y = factor(period), fill = factor(period)) +
    geom_density_ridges(alpha = .2) +
    geom_vline(
        xintercept = current_sales,
        linewidth = 2,
        alpha = .2,
        color = "gray50"
    ) +
    scale_x_continuous(
        labels = scales::dollar_format(scale = 1e-6, suffix = " M"),
        breaks = 1e6 * 0:20
    ) +
    labs(x = "Expected market value", y = "Predictor period length (in days)") +
    theme(legend.position = "none")

ggsave("montecarlo/mc-par6.png")
