message("more-listing-rate-pred.r")
library(tidyverse)
library(lubridate)
theme_set(theme_light())

load("Rdata/homesales.Rdata")

period_list <- sort(c(60, 180, 360))

homesale_history <- 
    homesales %>%
    filter(listingdate > today() - days(360), hometype == "residential") %>%
    mutate(y = 1, 
           cumulative_listing = cumsum(y),
           cumulative_market_value = cumsum(amount))


predict_listing_rate <- function(tbl, period) {



    tbl <- dplyr::rename(tbl, date = listingdate)

    pred_data <- tibble::tibble(date = c(
        lubridate::today() - days(period),
        lubridate::ceiling_date(today(), unit = "year")
    ))

     history <-
         tbl %>%
         filter(date > today() - days(period)) #%>%
    #     group_by(hometype) %>%
    #     mutate(
    #         counter = 1,
    #         totalamount = cumsum(counter),
    #         .groups = "drop"
    #     ) %>%
    #     select(date, hometype, totalamount)

    period_model <-
        history %>%
        group_by(hometype) %>%
        nest() %>%
        mutate(period_model = map(
            data,
            ~ lm(cumulative_listing ~ date, data = .x)
        ))

    # period_model_quality <-
    #     period_model %>%
    #     mutate(model_quality = map(period_model, ~ broom::glance(.x))) %>%
    #     unnest(model_quality) %>%
    #     select(r.squared)

    period_predict <-
        period_model %>%
        mutate(prediction = map(
            period_model,
            ~ broom::augment(.x, newdata = pred_data)
        )) %>%
        unnest(prediction) %>%
        select(-data, -period_model) #%>%
        # pivot_wider(
        #     names_from = date,
        #     values_from = .fitted
        #  ) #%>%
        # mutate(amount_listed = cur_data()[[2]] - cur_data()[[1]]) %>%
        # select(hometype, amount_listed)


    period_predict %>%
        mutate(period = period) #%>%
        # bind_cols(r.squared = period_model_quality$r.squared)
}

year_end_outcome <-
    map_df(period_list, ~ predict_listing_rate(homesale_history, .x)) %>%
    filter(hometype == "residential") #%>%
    #mutate(listingdate = date - days(period))

year_start <- approx(homesale_history$listingdate, homesale_history$cumulative_listing, floor_date(today(), unit = "year"))$y


homesales %>%
    filter(listingdate > today() - max(period_list), 
            hometype == "residential") %>%
    arrange(listingdate) %>%
    mutate(y = 1,
           cumulative_listing = cumsum(y)) %>%
    mutate(cumulative_listing = cumulative_listing - year_start) %>%
    ggplot() +
    aes(listingdate, cumulative_listing) +
    geom_point() +
    scale_y_continuous(
        labels = scales::comma_format(),
        breaks = 10 * 0:10
    ) +
    labs(
        x = "date",
        y = "cumulative listings",
        fill = "Hometype",
        title = paste("Listing rate prediction for end of year", year(today()))
    ) +
    theme(legend.position = "none") +
    geom_line(data = year_end_outcome %>% mutate(.fitted = .fitted - year_start), aes(x = date, y = .fitted, color = factor(period))) 

# ggsave("predictions/horizon-listings-rate.png", plot = p2)





predict_market_value <- function(tbl, period) {



    tbl <- dplyr::rename(tbl, date = saledate) %>% filter(!is.na(date))

    pred_data <- tibble::tibble(date = c(
        lubridate::today() - days(period),
        lubridate::ceiling_date(today(), unit = "year")
    ))

     history <-
         tbl %>%
         filter(date > today() - days(period)) #%>%
    #     group_by(hometype) %>%
    #     mutate(
    #         counter = 1,
    #         totalamount = cumsum(counter),
    #         .groups = "drop"
    #     ) %>%
    #     select(date, hometype, totalamount)

    period_model <-
        history %>%
        group_by(hometype) %>%
        nest() %>%
        mutate(period_model = map(
            data,
            ~ lm(cumulative_market_value ~ date, data = .x)
        ))

    # period_model_quality <-
    #     period_model %>%
    #     mutate(model_quality = map(period_model, ~ broom::glance(.x))) %>%
    #     unnest(model_quality) %>%
    #     select(r.squared)

    period_predict <-
        period_model %>%
        mutate(prediction = map(
            period_model,
            ~ broom::augment(.x, newdata = pred_data)
        )) %>%
        unnest(prediction) %>%
        select(-data, -period_model) #%>%
        # pivot_wider(
        #     names_from = date,
        #     values_from = .fitted
        #  ) #%>%
        # mutate(amount_listed = cur_data()[[2]] - cur_data()[[1]]) %>%
        # select(hometype, amount_listed)


    period_predict %>%
        mutate(period = period) #%>%
        # bind_cols(r.squared = period_model_quality$r.squared)
}


year_end_outcome_value <-
    map_df(period_list, ~ predict_market_value(homesale_history, .x)) %>%
    filter(hometype == "residential") #%>%
    #mutate(listingdate = date - days(period))

year_start_value <- approx(homesale_history$listingdate, homesale_history$cumulative_market_value, floor_date(today(), unit = "year"))$y


homesales %>%
    filter(listingdate > today() - max(period_list), 
            hometype == "residential") %>%
    arrange(listingdate) %>%
    mutate(y = 1,
           cumulative_market_value = cumsum(amount)) %>%
    mutate(cumulative_market_value = cumulative_market_value - year_start_value) %>% 
    ggplot() +
    aes(listingdate, cumulative_market_value) +
    geom_point() +
    scale_y_continuous(
        labels = scales::dollar_format(),
        breaks = 10e6 * 0:10
    ) +
    labs(
        x = "date",
        y = "cumulative listings",
        fill = "Hometype",
        title = paste("Listing rate prediction for end of year", year(today()))
    ) +
    theme(legend.position = "none") +
    geom_line(data = year_end_outcome_value %>% mutate(.fitted = .fitted - year_start_value), aes(x = date, y = .fitted, color = factor(period))) 

# ggsave("predictions/horizon-listings-rate.png", plot = p2)
