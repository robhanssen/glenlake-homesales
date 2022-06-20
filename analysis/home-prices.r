library(tidyverse)
library(lubridate)
theme_set(theme_light())

load("Rdata/homesales.Rdata")

quarter_summary <-
    homesales %>%
    filter(!is.na(saledate), !is.na(amount)) %>%
    mutate(quarter = quarter(saledate)) %>%
    arrange(saledate) %>%
    group_by(saleyear, quarter) %>%
    summarize(
        mean_amount = mean(amount),
        median_amount = median(amount),
        min_amount = min(amount),
        max_amount = max(amount),
        n = n(),
        sd = sd(amount, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(
        date = ymd(saleyear * 1e4 + quarter * 3 * 1e2 + 01) - days(15),
        stderr = qt(0.05 / 2, df = n - 1, lower.tail = FALSE) * sd / sqrt(n - 1), # nolint
        .upper = mean_amount + stderr,
        .lower = mean_amount - stderr
    )

lowest_amount <- with(
    homesales,
    mean(amount[saleyear == 2017],
        na.rm = TRUE
    )
)

quarter_summary %>%
    ggplot() +
    aes(x = date) +
    geom_col(aes(y = mean_amount), alpha = .5, color = "gray50") +
    geom_ribbon(aes(
        ymin = min_amount,
        ymax = max_amount,
        y = NULL
    ),
    alpha = .2
    ) +
    geom_errorbar(aes(ymin = .lower, ymax = .upper), width = 20) +
    scale_y_continuous(
        labels = scales::dollar_format(),
        limits = c(0, NA),
        sec.axis = sec_axis(~ . / lowest_amount - 1,
            labels = scales::percent_format(),
            name = "Relative to 2017 mean sale amount"
        )
    ) +
    labs(
        y = "Sale price (in USD)",
        x = "Date by year/quarter",
        caption = "Ribbon represents highest and lower sale price per quarter. Error bars are 95% confidence interval" # nolint
    ) +
    geom_hline(yintercept = lowest_amount, alpha = .7, lty = 3)