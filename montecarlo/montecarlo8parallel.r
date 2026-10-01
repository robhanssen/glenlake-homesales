library(tidyverse)
library(ggplot2)
library(ggridges)
library(patchwork)
library(furrr)
theme_set(theme_light())

plan(multisession)

load("Rdata/homesales.Rdata")

period_list <- c(120, 180, 360, 720)
niter_default <- 10000


make_price_cdf_plot <- function(dat, period) {

    dat_adj <- dat %>%
        filter(saledate > today() - days(period))

    prices <- log(as.numeric(dat_adj$amount))

    pars <- price_estimator(dat, period)

    mu_hat <- pars["mean0"]
    sd_hat <- pars["sd0"]

    cdf_emp <- ecdf(prices)

    plot_df <- tibble(
        t = seq(min(prices),
                max(prices),
                0.01)
    ) %>%
        mutate(
            empirical = cdf_emp(t),
            fitted = pnorm(
                t,
                mean = mu_hat,
                sd = sd_hat
            )
        ) %>%
        pivot_longer(
            c(empirical, fitted),
            names_to = "source",
            values_to = "cdf"
        )

    ggplot(plot_df,
           aes(exp(t), cdf, color = source)) +
        geom_line(linewidth = 1.1) +
        scale_color_manual(
            values = c(
                empirical = "black",
                fitted = "blue"
            )
        ) +
        scale_x_continuous(
            labels = scales::dollar_format()
        ) +
        labs(
            title = paste0(period, "-day window"),
            x = "Sale price",
            y = "CDF",
            color = NULL
        ) +
        theme_light()
}


library(tidyverse)
library(patchwork)

make_time_cdf_plot <- function(dat, period) {

    dat_adj <- dat %>%
        filter(saledate > today() - days(period)) %>%
        arrange(saledate)

    diffs <- as.numeric(diff(dat_adj$saledate))

    lambda_hat <- time_estimator(
        dat,
        saledate,
        period
    )[[1]]

    cdf_emp <- ecdf(diffs)

    plot_df <- tibble(
        t = seq(0, max(diffs), 1)
    ) %>%
        mutate(
            empirical = cdf_emp(t),
            fitted = pexp(t, rate = lambda_hat)
        ) %>%
        pivot_longer(
            c(empirical, fitted),
            names_to = "source",
            values_to = "cdf"
        )

    ggplot(plot_df,
           aes(t, cdf, color = source)) +
        geom_line(linewidth = 1.1) +
        scale_color_manual(
            values = c(
                empirical = "black",
                fitted = "red"
            )
        ) +
        labs(
            title = paste0(period, "-day window"),
            x = "Days between sales",
            y = "CDF",
            color = NULL
        ) +
        theme_light()
}



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

simul_sale <- function(dat, period, n_iter = niter_default) {
    sale_time_est <- time_estimator(dat, saledate, period)
    price_est <- price_estimator(dat, period = period)

    last_sale <- max(dat$saledate, na.rm = TRUE)

    simul_sale_values <- furrr::future_map(seq_len(n_iter), \(n) {
        sale_simul <- round(rexp(365, rate = sale_time_est))
        price_simul <- exp(rnorm(365, mean = price_est["mean0"], sd = price_est["sd0"]))

        next_sales <- last_sale + cumsum(sale_simul)

        est_sales_num <- length(next_sales[next_sales < ceiling_date(today(), unit = "year")])

        total_simul_sales <-
            sum(price_simul[seq_len(est_sales_num)])

        list(price = total_simul_sales, number = est_sales_num)
    }, .options = furrr_options(seed = TRUE))

    simul_sale_values
}

current_sales <- sum(homesales$amount[year(homesales$saledate) == year(today())], na.rm = TRUE)

current_num <- sum(!is.na(homesales$amount[year(homesales$saledate) == year(today())]))

multip <- future_map_dfr(period_list, \(p) {
    tibble(
        period = p,
        out = simul_sale(homesales, p, n_iter = niter_default)
    )
}, .options = furrr_options(seed = TRUE)) %>%
    unnest_wider(out) %>%
    mutate(
        price = price + current_sales,
        number = number + current_num
    )

dist_g <-
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
        breaks = 2e6 * 0:20
    ) +
    labs(x = "Expected market value", y = "Predictor period length (in days)") +
    theme(legend.position = "none")

num_g <-
    ggplot(multip) +
    aes(x = number, y = factor(period), fill = factor(period)) +
    geom_density_ridges(alpha = .2) +
    geom_vline(
        xintercept = current_num,
        linewidth = 2,
        alpha = .2,
        color = "gray50"
    ) +
    scale_x_continuous(
        # labels = scales::dollar_format(scale = 1e-6, suffix = " M"),
        # breaks = 1e6 * 0:20
    ) +
    labs(x = "Expected sales count", y = "Predictor period length (in days)") +
    theme(legend.position = "none")

time_plots <- map(
    period_list,
    ~ make_time_cdf_plot(homesales, .x)
)

price_plots <- map(
    period_list,
    ~ make_price_cdf_plot(homesales, .x)
)

cdf_dashboard <-
    wrap_plots(time_plots, ncol = 2) /
    wrap_plots(price_plots, ncol = 2) +
    plot_annotation(
        title = "Empirical vs Fitted Distribution Functions",
        subtitle =
            "Exponential fit for inter-sale times; Lognormal fit for prices"
    )

ggsave(
    "montecarlo/cdf_diagnostics.png",
    cdf_dashboard,
    width = 12,
    height = 10
)

ggsave("montecarlo/mc-par7.png",
    plot = num_g + dist_g,
    width = 10, height = 6
)