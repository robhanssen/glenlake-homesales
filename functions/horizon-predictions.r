

predict_market_size <- function(tbl, period) {
    tbl <- dplyr::rename(tbl, date = saledate) %>% dplyr::arrange(date)

    pred_data <- tibble::tibble(date = c(
        lubridate::floor_date(today(), unit = "year"),
        lubridate::ceiling_date(today(), unit = "year")
    ))

    history <-
        tbl %>%
        filter(date > today() - days(period)) %>%
        mutate(
            totalamount = cumsum(amount),
            .groups = "drop"
        ) %>%
        select(date, totalamount)

    period_model <-
        history %>%
        nest(data = everything()) %>%
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
        mutate(amount_sold = pick(everything())[[2]] - pick(everything())[[1]]) %>%
        select(amount_sold)


    period_predict %>%
        mutate(period = period) %>%
        bind_cols(r.squared = period_model_quality$r.squared)
}

predict_listing_rate <- function(tbl, period) {
    tbl <- dplyr::rename(tbl, date = listingdate)

    pred_data <- tibble::tibble(date = c(
        lubridate::floor_date(today(), unit = "year"),
        lubridate::ceiling_date(today(), unit = "year")
    ))

    history <-
        tbl %>%
        filter(date > today() - days(period)) %>%
        mutate(
            counter = 1,
            totalamount = cumsum(counter),
            .groups = "drop"
        ) %>%
        select(date, totalamount)

    period_model <-
        history %>%
        nest(data = everything()) %>%
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
        mutate(amount_listed = pick(everything())[[2]] - pick(everything())[[1]]) %>%
        select(amount_listed)


    period_predict %>%
        mutate(period = period) %>%
        bind_cols(r.squared = period_model_quality$r.squared)
}

predict_sale_rate <- function(tbl, period) {
    tbl <- dplyr::rename(tbl, date = saledate) %>% dplyr::arrange(date)

    pred_data <- tibble::tibble(date = c(
        lubridate::floor_date(today(), unit = "year"),
        lubridate::ceiling_date(today(), unit = "year")
    ))

    history <-
        tbl %>%
        filter(date > today() - days(period)) %>%
        mutate(
            counter = 1,
            totalamount = cumsum(counter),
            .groups = "drop"
        ) %>%
        select(date, totalamount)

    period_model <-
        history %>%
        nest(data = everything()) %>%
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
        mutate(amount_listed = pick(everything())[[2]] - pick(everything())[[1]]) %>%
        select(amount_listed)


    period_predict %>%
        mutate(period = period) %>%
        bind_cols(r.squared = period_model_quality$r.squared)
}
