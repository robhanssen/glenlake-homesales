library(tidyverse)
library(ggridges)
library(furrr)
library(patchwork)

plan(multisession)

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
time_estimator <- function(dat, var, year, lambda = .1) {
    dat_adj <- dat %>%
        filter(year({{ var }}) == year) %>%
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
price_estimator <- function(dat, year, mean0 = 12.5, sd0 = .2) {
    dat_adj <- dat %>%
        filter(year(saledate) == year) %>%
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

years <- unique(year(homesales$saledate))
years <- years[!is.na(years)]

n_iter_default <- 250000
n_iter <- n_iter_default

years <- 2018:2023

sales_est_df <- map_df(years, \(y) {
    sale_time_est <- time_estimator(homesales, saledate, y) %>%
        coefficients(.)

    price_est <- price_estimator(homesales, y) %>% coefficients(.)

    simul_sale_values <- furrr::future_map(seq_len(n_iter), \(n) {
        sale_simul <- round(rexp(365, rate = sale_time_est))
        price_simul <- exp(rnorm(365, mean = price_est["mean0"], sd = price_est["sd0"]))

        next_sales <- cumsum(sale_simul)

        # est_sales_num <- length(next_sales[next_sales < ceiling_date(today(), unit = "year")])
        est_sales_num <- sum(next_sales < 365)

        total_simul_sales <-
            sum(price_simul[seq_len(est_sales_num)])

        # list(price = total_simul_sales, number = est_sales_num)
        tibble(year = y, price = total_simul_sales, number = est_sales_num)

    }, .options = furrr_options(seed = TRUE))
})

# price_estimator(homesales, 2020)

sales_est_df %>%
    ggplot(
        aes(x = price, y = factor(year), color = factor(year))
    ) + 
    geom_density_ridges() + 
    scale_x_continuous(
        label = scales::label_dollar()
    ) + 
    geom_vline(
        xintercept = real_sales$sales
    )

sales_est_df %>%
        reframe(
            sales = mean(price),
            .by = year
        ) 


real_sales <- 
    homesales %>% filter(saleyear %in% years) %>%
        reframe(
            sales = sum(amount, na.rm = TRUE),
            .by = saleyear
        ) %>%
        drop_na(saleyear)