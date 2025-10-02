library(tidyverse)

theme_set(theme_light())

load("Rdata/homesales.Rdata")


multi_sales <- homesales %>%
    count(address) %>%
    filter(n > 1)


change_data <- homesales %>%
    semi_join(multi_sales, by = join_by(address)) %>%
    arrange(saledate) %>%
    group_by(address) %>%
    mutate(
        timebetweensales = (saledate - lag(saledate)) / dyears(1),
        amount_change = amount - lag(amount),
        amount_rel_change = ifelse(is.na(lag(amount)), 1, amount / lag(amount)),
        amount_cagr = amount_rel_change^(1 / timebetweensales) - 1
    ) %>%
    ungroup()

change_data %>%
    filter(amount_cagr != 0) %>%
    ggplot(aes(x = saledate, y = amount_cagr)) +
    geom_smooth(method = "loess", se = TRUE, color = "gray50", lty = 2, fill = "gray80") +
    geom_point(show.legend = FALSE) +
    scale_y_continuous(
        labels = scales::label_percent()
    ) +
    labs(
        x = "Date of sale",
        y = "Compound average increase in price (in %)"
    )


homesales %>%
    semi_join(multi_sales, by = join_by(address)) %>%
    arrange(saledate) %>%
    group_by(address) %>%
    mutate(
        timebetweensales = (last(saledate) - first(saledate)) / dyears(1),
        amount_change = last(amount) - first(amount),
        amount_rel_change = last(amount) / first(amount),
        amount_cagr = amount_rel_change^(1 / timebetweensales) - 1
    ) %>%
    ungroup() %>%
    filter(amount_cagr != 0) %>%
    ggplot(aes(x = saledate, y = amount_cagr)) +
    geom_smooth(method = "loess", se = TRUE, color = "gray50", lty = 2, fill = "gray80") +
    geom_point(show.legend = FALSE) +
    scale_y_continuous(
        limits = c(0, NA),
        labels = scales::label_percent()
    ) +
    labs(
        x = "Date of sale",
        y = "Compound average increase in price (in %)"
    )




sale_dates <- c(today(), ymd(20101122))

(385000/210000)^(1/((sale_dates[1] - sale_dates[2])/dyears(1)))