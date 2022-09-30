library(tidyverse)
library(lubridate)
theme_set(theme_light())

load("Rdata/homesales.Rdata")

lowest_amount <- with(
    homesales,
    median(amount[saleyear == 2019 | saleyear == 2018],
        na.rm = TRUE
    )
)

qlabeled_homesales <-
    homesales %>%
    filter(!is.na(saledate), !is.na(amount)) %>%
    mutate(
        quarter = quarter(saledate),
        qlabel = paste0(saleyear, "Q", quarter),
        qlabel = factor(qlabel)
    )

qlabeled_homesales %>%
    ggplot() +
    aes(x = qlabel, y = amount) +
    geom_boxplot() +
    scale_x_discrete() +
    scale_y_continuous(
        labels = scales::dollar_format(),
        limits = c(0, NA),
        sec.axis = sec_axis(~ . / lowest_amount - 1,
            labels = scales::percent_format(),
            name = "Relative to 2018/19 median sale price"
        )
    ) +
    labs(
        y = "Sale price (in USD)",
        x = "Date by year/quarter",
    ) +
    geom_hline(yintercept = lowest_amount, alpha = .7, lty = 3) +
    annotate("text",
        x = last(qlabeled_homesales$qlabel),
        y = lowest_amount - 10000,
        hjust = "right",
        label = "2018/19 median sale price"
    ) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

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

quarter_summary %>%
    ggplot() +
    aes(x = date) +
    geom_line(aes(y = median_amount)) +
    scale_y_continuous(
        labels = scales::dollar_format(),
        limits = c(0, NA),
        sec.axis = sec_axis(~ . / lowest_amount - 1,
            labels = scales::percent_format(),
            name = "Relative to 2018/19 mean sale amount"
        )
    ) +
    labs(
        y = "Sale price (in USD)",
        x = "Date by year/quarter",
    ) +
    annotate("text",
        x = max(quarter_summary$date),
        y = lowest_amount - 10000,
        hjust = "right",
        label = "2018/19 median sale price"
    ) +
    geom_hline(yintercept = lowest_amount, alpha = .7, lty = 3)