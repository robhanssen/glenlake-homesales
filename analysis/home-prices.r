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

quarter_plot <- quarter_summary %>%
    ggplot() +
    aes(x = date) +
    geom_line(aes(y = median_amount)) +
    scale_y_continuous(
        labels = scales::dollar_format(),
        limits = c(0, 2 * lowest_amount),
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


lowest_amount <- with(
    homesales,
    median(amount[(saleyear == 2019 | saleyear == 2018) & hometype != "townhome"],
        na.rm = TRUE
    )
)


amount2022 <- with(
    homesales,
    median(amount[saleyear == 2022 & hometype != "townhome"],
        na.rm = TRUE
    )
)

# 5-color set from ColorBrewers
colors <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "blue")

chc <- scales::percent(amount2022 / lowest_amount - 1, prefix = "+")


homesales %>%
    filter(!is.na(saledate), hometype != "townhome", saleyear > 2017) %>%
    mutate(year = factor(saleyear)) %>%
    ggplot() +
    aes(x = amount, y = year, fill = year, color = year) +
    geom_vline(
        xintercept = c(lowest_amount, amount2022),
        lty = 1, size = 2, alpha = .2
    ) +
    ggridges::geom_density_ridges2(show.legend = FALSE, alpha = .4) +
    scale_x_continuous(
        labels = scales::dollar_format(),
        sec.axis = sec_axis(~ . / lowest_amount - 1,
            labels = scales::percent_format(),
        ),
        breaks = 1e5 * 0:10
    ) +

    labs(
        x = "Home sale price", y = NULL,
        title = "Changes in home sale prices in Glen Lake",
    ) +
    annotate("text", x = lowest_amount - 5e3, y = .75,
            label = "Median value\n2018/2019", hjust = 1) +
    annotate("text", x = amount2022 + 5e3, y = .75,
            label = glue::glue("Median value\n2022 ({chc})"), hjust = 0) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors)

# ggsave("graphs/saleamount-change.png", width = 8, height = 7)


homesales2018 <-
    homesales %>%
    filter(saleyear == 2018) %>%
    mutate(saleyear = 2017.5)

am_factor <- amount2022 / lowest_amount

homesales %>%
    filter(!is.na(saledate), hometype != "townhome", saleyear > 2017) %>%
    filter(saleyear %in% c(2018, 2022)) %>%
    bind_rows(homesales2018) %>%
    mutate(year = factor(saleyear)) %>%
    mutate(amount = case_when(
        year == 2018 ~ amount * am_factor,
        TRUE ~ amount
    )) %>%
    mutate(dyear = case_when(
        year == 2018.5 ~ "2018\n(actual)",
        year == 2018 ~ "2018\n(extrapolated)",
        TRUE ~ paste(year, "\n(actual)")
    )) %>%
    ggplot() +
    aes(x = amount, y = dyear, fill = year, color = year) +
    geom_vline(
        xintercept = c(lowest_amount, amount2022),
        lty = 1, size = 2, alpha = .2
    ) +
    ggridges::geom_density_ridges2(show.legend = FALSE, alpha = .4) +
    scale_x_continuous(
        labels = scales::dollar_format(),
        sec.axis = sec_axis(~ . / lowest_amount - 1,
            labels = scales::percent_format(),
        ),
        breaks = 1e5 * 0:10
    ) +
    labs(
        x = "Home sale price", y = NULL,
        title = "Changes in home sale prices in Glen Lake",
        subtitle = glue::glue("Prices in 2018 were extrapolated to 2022 by changing {chc}") # nolint
    ) +
    annotate("text",
        x = lowest_amount - 5e3, y = .75,
        label = "Median value\n2018/2019", hjust = 1
    ) +
    annotate("text",
        x = amount2022 + 5e3, y = .75,
        label = glue::glue("Median value\n2022 ({chc})"), hjust = 0
    ) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors)

# ggsave("graphs/saleamount-extrapolation.png", width = 8, height = 7)