library(tidyverse)
library(patchwork)
library(zoo)

theme_set(
    theme_light() +
        theme(
            plot.title.position = "plot",
            plot.title = element_text(hjust = 0),
            panel.grid.minor = element_blank(),
            legend.position = "none",
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0)
        )
)

load("Rdata/homesales.Rdata")

normal_date <- function(date, projected_year = year(today())) {
    y <- lubridate::year(date)
    date + lubridate::years(projected_year - y)
}


full_date_range <-
    tibble(
        date = seq(
            floor_date(min(homesales$listingdate, na.rm = TRUE), "year"),
            ceiling_date(max(homesales$listingdate, na.rm = TRUE), "year") - days(1),
            by = "day"
        )
    )

cu_listing <-
    homesales %>%
    arrange(listingdate) %>%
    group_by(listingyear) %>%
    mutate(
        y = 1,
        cu_listing = cumsum(y),
    ) %>%
    ungroup() %>%
    select(listingdate, cu_listing) %>%
    full_join(full_date_range, by = c("listingdate" = "date")) %>%
    arrange(listingdate, cu_listing) %>%
    mutate(
        date = normal_date(listingdate),
        cu_listing = case_when(
            str_detect(date, "-01-01") & is.na(cu_listing) ~ 0,
            TRUE ~ cu_listing
        )
    ) %>%
    fill(cu_listing, .direction = "down") %>%
    filter(!str_detect(date, "-02-29"))

listings_g <-
    cu_listing %>%
    filter(year(listingdate) != year(today())) %>%
    summarize(
        min = min(cu_listing),
        max = max(cu_listing),
        med = median(cu_listing),
        .by = date
    ) %>%
    ggplot(aes(x = date)) +
    geom_step(aes(y = min), color = "tan") +
    geom_step(aes(y = max), color = "tan") +
    geom_step(aes(y = med), color = "tan", linetype = 3) +
    geom_step(
        data = cu_listing %>%
            filter(
                year(listingdate) == year(today()),
                date <= today()
            ),
        aes(x = date, y = cu_listing),
        color = "darkgreen"
    ) +
    labs(
        x = "Date", y = "Minimum and maximum cumulative listings",
        title = "Listing comparison"
    ) +
    scale_x_date(
        date_labels = "%b"
    ) +
    scale_y_continuous(
        limits = c(0, 60)
    )





cu_sales <-
    homesales %>%
    filter(!is.na(saledate)) %>%
    arrange(saledate) %>%
    group_by(saleyear) %>%
    mutate(
        y = 1,
        cu_listing = cumsum(y),
    ) %>%
    ungroup() %>%
    select(saledate, cu_listing) %>%
    full_join(full_date_range, by = c("saledate" = "date")) %>%
    arrange(saledate, cu_listing) %>%
    mutate(
        date = normal_date(saledate),
        cu_listing = case_when(
            str_detect(date, "-01-01") & is.na(cu_listing) ~ 0,
            TRUE ~ cu_listing
        )
    ) %>%
    fill(cu_listing, .direction = "down") %>%
    filter(!str_detect(date, "-02-29"))

sales_g <-
    cu_sales %>%
    filter(year(saledate) != year(today())) %>%
    summarize(
        min = min(cu_listing),
        max = max(cu_listing),
        med = median(cu_listing),
        .by = date
    ) %>%
    ggplot(aes(x = date)) +
    geom_step(aes(y = min), color = "tan") +
    geom_step(aes(y = max), color = "tan") +
    geom_step(aes(y = med), color = "tan", linetype = 3) +
    geom_step(
        data = cu_sales %>%
            filter(
                year(saledate) == year(today()),
                date <= today()
            ),
        aes(x = date, y = cu_listing),
        color = "darkgreen"
    ) +
    labs(
        x = "Date", y = "Minimum and maximum cumulative sales",
        title = "Sales comparison"
    ) +
    scale_x_date(
        date_labels = "%b"
    ) +
    scale_y_continuous(
        limits = c(0, 60)
    )



cu_market <-
    homesales %>%
    filter(!is.na(saledate), !is.na(amount)) %>%
    arrange(saledate) %>%
    group_by(saleyear) %>%
    mutate(
        # y = 1,
        cu_market = cumsum(amount),
    ) %>%
    ungroup() %>%
    select(saledate, cu_market) %>%
    full_join(full_date_range, by = c("saledate" = "date")) %>%
    arrange(saledate, cu_market) %>%
    mutate(
        date = normal_date(saledate),
        cu_market = case_when(
            str_detect(date, "-01-01") & is.na(cu_market) ~ 0,
            TRUE ~ cu_market
        )
    ) %>%
    fill(cu_market, .direction = "down") %>%
    filter(!str_detect(date, "-02-29"))

market_g <-
    cu_market %>%
    filter(year(saledate) != year(today())) %>%
    summarize(
        min = min(cu_market),
        max = max(cu_market),
        med = median(cu_market),
        .by = date
    ) %>%
    ggplot(aes(x = date)) +
    geom_step(aes(y = min), color = "tan") +
    geom_step(aes(y = max), color = "tan") +
    geom_step(aes(y = med), color = "tan", linetype = 3) +
    geom_step(
        data = cu_market %>%
            filter(
                year(saledate) == year(today()),
                date <= today()
            ),
        aes(x = date, y = cu_market),
        color = "darkgreen"
    ) +
    labs(
        x = "Date", y = "Minimum and maximum cumulative sales",
        title = "Market size comparison"
    ) +
    scale_x_date(
        date_labels = "%b"
    ) +
    scale_y_continuous(
        labels = scales::label_dollar(scale = 1e-6, suffix = " M"),
        limits = c(0, NA)
    )


p <- listings_g + sales_g + market_g & theme(axis.title.x = element_blank())

ggsave("graphs/listing_sales_comparison.png",
    width = 14, height = 5,
    plot = p
)
