library(tidyverse)
library(lubridate)
theme_set(theme_light())

load("Rdata/homesales.Rdata")

normal_date <- function(date, projected_year = year(today())) {
        y <- lubridate::year(date)
        date + lubridate::years(projected_year - y)
}

inventorycalc <-
        homesales %>%
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
        ) %>%
        select(date, inventory, year)


all_dates <- tibble(
        date = seq(
                min(inventorycalc$date),
                max(inventorycalc$date),
                by = "day"
        )
)

inventory_means <-
        inventorycalc %>%
        right_join(all_dates) %>%
        arrange(date) %>%
        fill(inventory) %>%
        mutate(display_date = normal_date(date)) %>%
        group_by(display_date) %>%
        summarize(
                mn_inventory = mean(inventory, na.rm = TRUE),
                md_inventory = median(inventory,
                        na.rm = TRUE,
                        .groups = "drop"
                )
        ) %>%
        mutate(across(ends_with("inventory"),
                .names = "{.col}_soft",
                ~ zoo::rollmean(.x, 7, na.pad = TRUE, align = "center")
        )) %>%
        drop_na()



inventorycalc %>%
        ggplot() +
        aes(x = date, y = inventory, color = factor(year)) +
        geom_line() +
        labs(
                x = "Date",
                y = "Home inventory",
                caption = caption,
                title = "Inventory of homes for sale in Glen Lake"
        ) +
        scale_x_date(date_breaks = "1 year", date_label = "%Y") +
        theme(legend.position = "none")

ggsave("graphs/home-inventory-fullrange.png", width = 6, height = 6)

year_range <- unique(homesales$listingyear)
year_length <- length(year_range)
color_range <- c(rep("gray50", year_length - 1), "black")
alpha_range <- c(rep(.11, year_length - 2), .5, .9)

inventorycalc %>%
        mutate(display_date = normal_date(date, max_year)) %>%
        ggplot() +
        aes(
                x = display_date,
                y = inventory,
                color = factor(year),
                alpha = factor(year)
        ) +
        geom_line(
                data = inventory_means,
                aes(
                        y = md_inventory_soft,
                        color = NULL,
                        alpha = NULL
                ),
                lty = 2,
                alpha = max(alpha_range)
        ) +
        geom_line() +
        geom_point(data = . %>% filter(year(date) == max_year)) +
        labs(
                x = "Date",
                y = "Home inventory",
                color = "Year",
                caption = caption,
                title = "Inventory of homes for sale in Glen Lake"
        ) +
        scale_x_date(date_breaks = "3 months", date_label = "%b") +
        scale_color_manual(values = color_range) +
        scale_alpha_manual(values = alpha_range) +
        theme(legend.position = "none")

ggsave("graphs/home-inventory.png", width = 8, height = 8)