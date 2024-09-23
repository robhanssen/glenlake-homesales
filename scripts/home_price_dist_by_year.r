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

# estimator of sale amount modeled as lognormal distribution
price_estimator <- function(dat, saleyear, mean0 = 12.3, sd0 = .2) {
    dat_adj <- dat %>%
        pull(amount)

    mean0_est <- mean(log(dat_adj))

    prices <- log(as.numeric(dat_adj))

    p_cdf <- ecdf(prices)

    est_tibble <- tibble(
        t = seq(min(prices), max(prices), .01),
        e = p_cdf(t)
    )

    mod <- nls(
        e ~ pnorm(t, mean0, sd0),
        start = list(mean0 = mean0_est, sd0 = sd0),
        data = est_tibble
    )

    mod
}

time_estimator <- function(dat, saleyear, lambda = .1) {
    dat_adj <- dat %>%
        # filter(saleyear == saleyear) %>%
        arrange(saledate) %>%
        pull(saledate)
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




dist_g <-
    homesales %>%
    nest(data = !saleyear) %>%
    drop_na() %>%
    mutate(fit_amount = map(data, price_estimator)) %>%
    mutate(outmodel = map(fit_amount, broom::augment)) %>%
    unnest(outmodel) %>%
    ggplot(aes(x = exp(t), y = .fitted, color = factor(saleyear))) +
    geom_line() +
    geom_point(aes(y = e), shape = 1) +
    scale_x_continuous(
        breaks = 1e3 * seq(100, 1000, 100),
        labels = scales::label_dollar(scale = 1e-3, suffix = "K")
    ) +
    scale_y_continuous(labels = scales::label_percent()) +
    facet_wrap(vars(saleyear), scale = "free_x") +
    labs(
        x = "", y = ""
    )

mean_price_g <-
    homesales %>%
    nest(data = !saleyear) %>%
    drop_na() %>%
    mutate(fit_amount = map(data, price_estimator)) %>%
    mutate(outmodel = map(fit_amount, broom::tidy)) %>%
    unnest(outmodel) %>%
    filter(term == "mean0") %>%
    mutate(
        e_high = estimate + 1.96 * std.error,
        e_low = estimate - 1.96 * std.error,
        e_mid = estimate,
        across(starts_with("e_"), exp)
    ) %>%
    ggplot(
        aes(y = e_mid, x = factor(saleyear))
    ) +
    geom_point() +
    geom_errorbar(aes(ymin = e_low, ymax = e_high), width = .2) +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(
        x = "Year of sale",
        y = "Average sale price"
    )

time_dist_g <-
    homesales %>%
    nest(data = !saleyear) %>%
    drop_na() %>%
    mutate(fit_amount = map(data, time_estimator)) %>%
    mutate(outmodel = map(fit_amount, broom::augment)) %>%
    unnest(outmodel) %>%
    ggplot(aes(x = t, y = .fitted, color = factor(saleyear))) +
    geom_line() +
    geom_point(aes(y = e), shape = 1) +
    scale_x_continuous(
        # breaks = 1e3 * seq(100, 1000, 100),
        # labels = scales::label_dollar(scale = 1e-3, suffix = "K")
    ) +
    scale_y_continuous(labels = scales::label_percent()) +
    facet_wrap(vars(saleyear), scale = "free_x") +
    labs(
        x = "", y = ""
    )

mean_time_g <-
    homesales %>%
    nest(data = !saleyear) %>%
    drop_na() %>%
    mutate(fit_amount = map(data, time_estimator)) %>%
    mutate(outmodel = map(fit_amount, broom::tidy)) %>%
    unnest(outmodel) %>%
    filter(term == "lambda0") %>%
    mutate(
        e_high = estimate + 1.96 * std.error,
        e_low = estimate - 1.96 * std.error,
        e_mid = estimate,
        across(starts_with("e_"), ~ 1 / .x)
    ) %>%
    ggplot(
        aes(y = e_mid, x = factor(saleyear))
    ) +
    geom_point() +
    geom_errorbar(aes(ymin = e_low, ymax = e_high), width = .2) +
    scale_y_continuous(labels = scales::label_number(), limits = c(0, NA)) +
    labs(
        x = "Year of sale",
        y = "Mean days between sales"
    )

ggsave("graphs/mean_by_year_dist.png",
    width = 14, height = 10,
    plot = (dist_g + mean_price_g) / (time_dist_g + mean_time_g)
)
