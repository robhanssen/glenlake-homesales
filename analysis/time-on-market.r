library(tidyverse)
library(lubridate)
theme_set(theme_light())

load("Rdata/homesales.Rdata")

quarter_summary <-
    homesales %>%
    filter(!is.na(saledate), !is.na(timeonmarket)) %>%
    mutate(quarter = quarter(saledate)) %>%
    arrange(saledate) %>%
    group_by(saleyear, quarter) %>%
    summarize(
        mean_amount = mean(timeonmarket),
        median_amount = median(timeonmarket),
        min_amount = min(timeonmarket),
        max_amount = max(timeonmarket),
        .groups = "drop"
    ) %>%
    mutate(date = ymd(saleyear * 1e4 + quarter * 3 * 1e2 + 01) - days(15),
          across(ends_with("amount"), ~ as.numeric(.x)))

lowest_amount <- with(
    homesales,
    as.numeric(median(timeonmarket[saleyear == 2018],
        na.rm = TRUE
    )
    )
)

quarter_summary %>%
    ggplot() +
    aes(x = date) +
    geom_col(aes(y = median_amount), alpha = .5, color = "gray50") +
    geom_ribbon(aes(ymin = min_amount, ymax = max_amount, y = NULL), alpha = .2) +
    scale_x_date(date_breaks = "1 year", date_label = "%Y") +
    scale_y_continuous(
        labels = scales::number_format(),
        limits = c(0, NA),
        sec.axis = sec_axis(~ . / lowest_amount - 1,
                            labels = scales::percent_format(),
                            name = "Relative to 2018 median time on market")
    ) +
    labs(
        y = "Time on market (in days)",
        x = "Date by year/quarter",
        caption = "Ribbon represents highest and lower time per quarter"
    ) + 
    geom_hline(yintercept = lowest_amount, alpha = .7, lty = 3)