message("more-predictions.r")
library(tidyverse)
library(lubridate)
theme_set(theme_light())

load("Rdata/homesales.Rdata")

period_list <- sort(c(60, 120, 90 * 2:4))

predict_market_size <- function(tbl, period) {
    tbl <- dplyr::rename(tbl, date = saledate) %>% dplyr::arrange(date)

    pred_data <- tibble::tibble(date = c(
        lubridate::floor_date(today(), unit = "year"),
        lubridate::ceiling_date(today(), unit = "year")
    ))

    history <-
        tbl %>%
        filter(date > today() - days(period)) %>%
        group_by(hometype) %>%
        mutate(
            totalamount = cumsum(amount),
            .groups = "drop"
        ) %>%
        select(date, hometype, totalamount)

    period_model <-
        history %>%
        group_by(hometype) %>%
        nest() %>%
        mutate(period_model = map(
            data,
            ~ lm(totalamount ~ date, data = .x)
        ))

    period_model_quality <-
        period_model %>%
        mutate(model_quality = map(period_model, ~ broom::glance(.x))) %>%
        unnest(model_quality) %>%
        select(r.squared)

    period_predict <-
        period_model %>%
        mutate(prediction = map(
            period_model,
            ~ broom::augment(.x, newdata = pred_data)
        )) %>%
        unnest(prediction) %>%
        select(-data, -period_model) %>%
        pivot_wider(
            names_from = date,
            values_from = .fitted
        ) %>%
        mutate(amount_sold = cur_data()[[2]] - cur_data()[[1]]) %>%
        select(hometype, amount_sold)


    period_predict %>%
        mutate(period = period) %>%
        bind_cols(r.squared = period_model_quality$r.squared)
}

homesales_adjusted <- homesales %>% filter(!is.na(amount))

p1 <-
    map_df(period_list, ~ predict_market_size(homesales_adjusted, .x)) %>%
    ggplot() +
    aes(factor(period), amount_sold, fill = hometype) +
    geom_col() +
    scale_y_continuous(
        labels = scales::dollar_format(),
        breaks = 2e6 * 0:10
    ) +
    labs(
        x = "# of days used in prediction",
        y = "Predicted market size ($)",
        fill = "Hometype",
        title = paste("Market size prediction for end of year", year(today()))
    ) +
    theme(legend.position = "none")

ggsave("predictions/horizon-market-value.png", plot = p1)


#
# listing rate
#

predict_listing_rate <- function(tbl, period) {
    tbl <- dplyr::rename(tbl, date = listingdate)

    pred_data <- tibble::tibble(date = c(
        lubridate::floor_date(today(), unit = "year"),
        lubridate::ceiling_date(today(), unit = "year")
    ))

    history <-
        tbl %>%
        filter(date > today() - days(period)) %>%
        group_by(hometype) %>%
        mutate(
            counter = 1,
            totalamount = cumsum(counter),
            .groups = "drop"
        ) %>%
        select(date, hometype, totalamount)

    period_model <-
        history %>%
        group_by(hometype) %>%
        nest() %>%
        mutate(period_model = map(
            data,
            ~ lm(totalamount ~ date, data = .x)
        ))

    period_model_quality <-
        period_model %>%
        mutate(model_quality = map(period_model, ~ broom::glance(.x))) %>%
        unnest(model_quality) %>%
        select(r.squared)

    period_predict <-
        period_model %>%
        mutate(prediction = map(
            period_model,
            ~ broom::augment(.x, newdata = pred_data)
        )) %>%
        unnest(prediction) %>%
        select(-data, -period_model) %>%
        pivot_wider(
            names_from = date,
            values_from = .fitted
        ) %>%
        mutate(amount_listed = cur_data()[[2]] - cur_data()[[1]]) %>%
        select(hometype, amount_listed)


    period_predict %>%
        mutate(period = period) %>%
        bind_cols(r.squared = period_model_quality$r.squared)
}

p2 <-
    map_df(period_list, ~ predict_listing_rate(homesales, .x)) %>%
    ggplot() +
    aes(factor(period), amount_listed, fill = hometype) +
    geom_col() +
    scale_y_continuous(
        labels = scales::comma_format(),
        breaks = 10 * 0:10
    ) +
    labs(
        x = "# of days used in prediction",
        y = "Predicted number of listings",
        fill = "Hometype",
        title = paste("Listing rate prediction for end of year", year(today()))
    ) +
    theme(legend.position = "none")

ggsave("predictions/horizon-listings-rate.png", plot = p2)



#
# sale rate
#

predict_sale_rate <- function(tbl, period) {
    tbl <- dplyr::rename(tbl, date = saledate) %>% dplyr::arrange(date)

    pred_data <- tibble::tibble(date = c(
        lubridate::floor_date(today(), unit = "year"),
        lubridate::ceiling_date(today(), unit = "year")
    ))

    history <-
        tbl %>%
        filter(date > today() - days(period)) %>%
        group_by(hometype) %>%
        mutate(
            counter = 1,
            totalamount = cumsum(counter),
            .groups = "drop"
        ) %>%
        select(date, hometype, totalamount)

    period_model <-
        history %>%
        group_by(hometype) %>%
        nest() %>%
        mutate(period_model = map(
            data,
            ~ lm(totalamount ~ date, data = .x)
        ))

    period_model_quality <-
        period_model %>%
        mutate(model_quality = map(period_model, ~ broom::glance(.x))) %>%
        unnest(model_quality) %>%
        select(r.squared)

    period_predict <-
        period_model %>%
        mutate(prediction = map(
            period_model,
            ~ broom::augment(.x, newdata = pred_data)
        )) %>%
        unnest(prediction) %>%
        select(-data, -period_model) %>%
        pivot_wider(
            names_from = date,
            values_from = .fitted
        ) %>%
        mutate(amount_listed = cur_data()[[2]] - cur_data()[[1]]) %>%
        select(hometype, amount_listed)


    period_predict %>%
        mutate(period = period) %>%
        bind_cols(r.squared = period_model_quality$r.squared)
}

p3 <-
    map_df(period_list, ~ predict_sale_rate(homesales, .x)) %>%
    ggplot() +
    aes(factor(period), amount_listed, fill = hometype) +
    geom_col() +
    scale_y_continuous(
        labels = scales::comma_format(),
        breaks = 10 * 0:10
    ) +
    labs(
        x = "# of days used in prediction",
        y = "Predicted number of sales",
        fill = "Hometype",
        title = paste("Sale rate prediction for end of year", year(today()))
    ) +
    theme(legend.position = "none")

ggsave("predictions/horizon-sale-rate.png", plot = p3)