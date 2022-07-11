library(tidyverse)
library(lubridate)
source("functions/config.r")
theme_set(theme_light())

load("Rdata/homesales.Rdata")

homesales_filtered <-
    homesales %>%
    filter(!is.na(saledate))

homes_sold_last_12months <- function(tbl, date) {
    date_year_ago <- date - lubridate::years(1)
    sale_data <-
        tbl %>%
        filter(saledate > date_year_ago, saledate <= date)

    tibble::tibble(date = date, homesold = nrow(sale_data))
}

homes_listed_last_12months <- function(tbl, date) {
    date_year_ago <- date - lubridate::years(1)
    listing_data <-
        tbl %>%
        filter(listingdate > date_year_ago, listingdate <= date)

    tibble::tibble(date = date, homeslisted = nrow(listing_data))
}


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

listed_last_year_by_month <-
    map_dfr(monthlist, ~ homes_listed_last_12months(homesales, .x))

listed_last_year_by_month %>%
    filter(date >= first(datelist) + years(1)) %>%
    ggplot() +
    aes(date, homeslisted) +
    geom_line() +
    geom_line(data = sold_last_year_by_month %>% filter(date >= first(datelist) + years(1)),
                aes(y = homesold), color = "red") +
    scale_x_date() +
    scale_y_continuous(limits = c(0, NA), breaks = 10 * 0:100) +
    labs(
        x = "Date",
        y = "Number of homes listed or sold in the last 12 months",
        title = "Glen Lake average home listings and sales in 12 months",
        caption = paste0(caption_source, "\nBlack line: listings; Red line: sales")
    )

ggsave("graphs/average-homesales-per-12-months.png")


current_market_size <-
    homesales %>%
    select(listingdate, saledate) %>%
    pivot_longer(everything(), names_to = "type", values_to = "date") %>%
    arrange(date) %>%
    filter(!is.na(date)) %>%
    mutate(y = ifelse(type == "listingdate", 1, -1)) %>%
    mutate(homesonmarket = cumsum(y)) %>%
    select(-y)

note_date <- last(datelist) - days(180)

# average residence time

sold_last_year_by_month %>%
    mutate(residencetime = 484 / homesold) %>%
    filter(date >= first(datelist) + years(1)) %>%
    ggplot() +
    aes(date, residencetime) +
    geom_line() +
    scale_x_date() +
    scale_y_continuous(limits = c(0, NA), breaks = 10 * 0:100) +
    labs(
        x = "Date",
        y = "Average residence time (in years)",
        title = "Glen Lake average residence time",
        caption = caption_source
    )

ggsave("graphs/average-residence-time.png", width = 8, height = 6)

# inventory time (smoothed by month)

current_market_size %>%
    mutate(month = floor_date(date, unit = "month")) %>%
    group_by(month) %>%
    summarize(
        homesonmarket = mean(homesonmarket, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    inner_join(sold_last_year_by_month, by = c("month" = "date")) %>%
    mutate(market_speed = homesonmarket / homesold * 12) %>%
    filter(month >= first(datelist) + years(1)) %>%
    ggplot() +
    aes(month, market_speed) +
    geom_line(color = "gray50", alpha = .5) +
    scale_x_date() +
    scale_y_continuous(limits = c(0, NA), breaks = 1 * 0:100) +
    geom_hline(yintercept = 6, lty = 2, color = "gray50") +
    geom_smooth(
        method = "loess",
        se = FALSE,
        lty = "dashed",
        color = "gray50"
    ) +
    labs(
        x = "Date",
        y = "Inventory rate (in months)",
        caption = caption_source
    ) +
    annotate("text", x = note_date, y = 6.5, label = "Buyer's Market") +
    annotate("text", x = note_date, y = 5.5, label = "Seller's Market")

ggsave("graphs/average-inventory-time.png", width = 8, height = 6)