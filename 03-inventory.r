library(tidyverse)
library(lubridate)
theme_set(theme_light())

load("Rdata/homesales.Rdata")

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
        )

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

ml <- floor(seq(0, 364, length.out = 12))

inventory_minmax <-
        inventorycalc %>%
        mutate(display_date = normal_date(date, max_year),
                mn = month(display_date)) %>%
        group_by(mn) %>%
        summarize(med_inv = median(inventory, na.rm = TRUE),
                mean_inv = mean(inventory, , na.rm = TRUE),
                .groups = "drop") %>% 
        mutate(display_date = as.Date(paste0(max_year, "-01-01")) + days(ml))


normal_date <- function(date, projected_year) {
        y <- lubridate::year(date)
        date + lubridate::years(projected_year - y)
}

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
                data = inventory_minmax,
                aes(
                        # ymin = med_inv,
                        y = mean_inv,
                        # y = NULL,
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
