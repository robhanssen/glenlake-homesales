library(tidyverse)
library(ggridges)
library(patchwork)

theme_set(
    theme_light() +
        theme(
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0),
            legend.position = "none",
            panel.grid.minor = element_blank()
        )
)


load("Rdata/homesales.Rdata")

period_list <- c(90, 120, 180, 360, 720)
niter_default <- 10000

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

    # coefficients(mod)
    mod
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

    # coefficients(mod)
    mod
}

time_diff <- function(dat, var, period, lambda = .1) {
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

    est_tibble
}

price_actual <- function(dat, period, mean0 = 12.7, sd0 = .2) {
    dat_adj <- dat %>%
        filter(saledate > today() - days(period)) %>%
        pull(amount)

    prices <- log(as.numeric(dat_adj))

    p_cdf <- ecdf(prices)

    est_tibble <- tibble(
        t = seq(min(prices), max(prices), .01),
        e = p_cdf(t)
    )

    est_tibble
}



# period_list <- seq(100, 2500, 300)
period_list <- 365 * 1:8

time_fit_g <-
    map_df(period_list, \(p) {
        actual <- time_diff(homesales, saledate, p)

        mod <- time_estimator(homesales, saledate, p) %>%
            broom::augment()

        inner_join(actual, mod) %>% mutate(period = p)
    }) %>%
    mutate(
        period = factor(period, ordered = TRUE, labels = paste(period_list, "days"))
    ) %>%
    ggplot(
        aes(x = t, color = period)
    ) +
    geom_point(aes(y = e), shape = 1) +
    geom_line(aes(y = .fitted)) +
    labs(
        x = "Day between sales",
        y = "CDF",
        title = "Fitting exponential distributions on the time between sales",
        subtitle = "For different time intervals ending today"
    ) +
    facet_wrap(vars(factor(period)))

map_df(period_list, \(p) {
    mod <- time_estimator(homesales, saledate, p) %>%
        broom::tidy() %>%
        mutate(period = p)
}) %>%
    mutate(
        time = 1 / estimate,
        time_low = 1 / (estimate - std.error),
        time_hi = 1 / (estimate + std.error)
    ) %>%
    ggplot(
        aes(x = time, y = factor(period))
    ) +
    geom_point() +
    geom_errorbar(
        aes(x = time, xmin = time_low, xmax = time_hi),
        width = .25
    ) +
    scale_x_continuous(
        limits = c(0, NA)
    ) +
    labs(
        x = "Average time (in days) between homesales",
        y = "Period over which is averaged (in days ago)"
    )




price_fit_g <-
    map_df(period_list, \(p) {
        actual <- price_actual(homesales, p)

        mod <- price_estimator(homesales, p) %>%
            broom::augment()

        inner_join(actual, mod) %>% mutate(period = p)
    }) %>%
    mutate(
        period = factor(period, ordered = TRUE, labels = paste(period_list, "days"))
    ) %>%
    ggplot(
        aes(x = exp(t), color = factor(period))
    ) +
    geom_point(aes(y = e)) +
    geom_line(aes(y = .fitted)) +
    labs(
        x = "Home prices (in 1000's US$)",
        y = "CDF",
        title = "Fitting lognormal distributions on home prices",
        subtitle = "For different time intervals ending today"
    ) +
    scale_x_continuous(
        labels = scales::label_currency(scale = 1e-3, prefix = "$ "),
        breaks = scales::pretty_breaks()
    ) +
    facet_wrap(vars(period))


map_df(period_list, \(p) {
    mod <- price_estimator(homesales, p) %>%
        broom::tidy() %>%
        mutate(period = p) %>%
        filter(term == "mean0")
}) %>%
    mutate(
        time = exp(estimate),
        time_low = exp(estimate - std.error),
        time_hi = exp(estimate + std.error)
    ) %>%
    ggplot(
        aes(x = time, y = factor(period))
    ) +
    geom_point() +
    geom_errorbar(
        aes(x = time, xmin = time_low, xmax = time_hi),
        width = .25
    ) +
    scale_x_continuous(
        # limits = c(0, NA)
        labels = scales::label_dollar()
    ) +
    labs(
        x = "Average sale price (in USD)",
        y = "Period over which is averaged (in days ago)",
        title = "Fitting lognormal distributions on the home prices",
        subtitle = "For different time intervals ending today"
    )

ggsave("graphs/time_price_fit.png", plot = time_fit_g + price_fit_g,
    width = 10, height = 6)


price_actual(homesales, 4500) %>%
    mutate(x = exp(t)) %>%
    ggplot(aes(x, e)) +
    geom_point()

price_estimator(homesales, 4500) %>%
    broom::augment() %>%
    mutate(x = exp(t)) %>%
    ggplot(aes(x, .fitted)) +
    geom_line() +
    geom_point(aes(y = e))
