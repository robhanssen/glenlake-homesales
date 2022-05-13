# inventory time

average_inventory_size <- function(tbl) {
    inventory <-
        tbl %>%
        select(address, listingdate, saledate) %>%
        pivot_longer(ends_with("date"),
            names_to = "type",
            values_to = "date"
        ) %>%
        mutate(y = ifelse(type == "listingdate", 1, -1)) %>%
        filter(!is.na(date)) %>%
        arrange(date) %>%
        mutate(
            inventory = cumsum(y),
            year = year(date)
        )

    fulldatarange <-
        tibble(date = with(
            inventory,
            seq(min(date),
                max(date),
                by = "day"
            )
        ))

    full_data <-
        full_join(fulldatarange, inventory) %>%
        fill(c(inventory, year), .direction = "down")

    full_data %>%
        group_by(year) %>%
        summarize(inventory = mean(inventory, na.rm = TRUE))
}


homes_sold_last_12months <- function(tbl, date) {
    date_year_ago <- date - lubridate::years(1)
    sale_data <-
        tbl %>%
        filter(saledate > date_year_ago, saledate <= date)

    tibble::tibble(date = date, homesold = nrow(sale_data))
}

average_inventory_time <- function(tbl) {
    homesales_filtered <-
        tbl %>%
        filter(!is.na(saledate))

    datelist <-
        with(
            homesales_filtered,
            sort(unique(c(listingdate, saledate, today())))
        )

    monthlist <- c(
        seq(
            floor_date(first(datelist), unit = "month"),
            floor_date(last(datelist), unit = "month"),
            "month"
        ),
        today()
    )

    sold_last_year_by_month <-
        map_dfr(monthlist, ~ homes_sold_last_12months(homesales_filtered, .x))

    current_market_size <-
        tbl %>%
        select(listingdate, saledate) %>%
        pivot_longer(everything(), names_to = "type", values_to = "date") %>%
        arrange(date) %>%
        filter(!is.na(date)) %>%
        mutate(y = ifelse(type == "listingdate", 1, -1)) %>%
        mutate(homesonmarket = cumsum(y)) %>%
        select(-y)

    current_market_size %>%
        mutate(month = floor_date(date, unit = "month")) %>%
        group_by(month) %>%
        summarize(
            homesonmarket = mean(homesonmarket, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        inner_join(sold_last_year_by_month, by = c("month" = "date")) %>%
        mutate(market_speed = homesonmarket / homesold * 12) %>%
        filter(month >= first(datelist) + years(1))
}