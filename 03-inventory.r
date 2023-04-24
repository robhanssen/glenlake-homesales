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

#
# CDF for inventory
#
#

pdf_norm <- function(x, mu, sig) {
        .5 * (1 + pracma::erf((x - mu) / (sqrt(2) * sig)))
}

pdf_lognorm <- function(x, mu, sig) {
    .5 * (1 + pracma::erf((log(x) - mu) / (sqrt(2) * sig)))
}

invsoft <-
        inventorycalc %>%
        right_join(all_dates) %>%
        arrange(date) %>%
        fill(inventory) %>%
        filter(year(date) > 2017)

minventory <- (min(invsoft$inventory) %/% 10) * 10
maxventory <- (max(invsoft$inventory) %/% 10 + 1) * 10

mean_inv <- mean((invsoft$inventory))
sd_inv <- sd((invsoft$inventory))

logmean_inv <- mean(log(invsoft$inventory))
logsd_inv <- sd(log(invsoft$inventory))


inv_cdf <-
        tibble(
                inv = seq(minventory, maxventory, 1),
                cdf = map_dbl(inv, ~ nrow(invsoft %>% filter(inventory <= .x)) / nrow(invsoft)), # nolint
                cdf_simul_log = map_dbl(inv, ~ pdf_lognorm(.x, logmean_inv, logsd_inv)), # nolint
                cdf_simul_norm = map_dbl(inv, ~ pdf_norm(.x, mean_inv, sd_inv)) # nolint
        )

mid_point <-
        tibble(y = c(.1, .5, .9, .99)) %>%
        mutate(x = map(
                y,
                ~ approx(inv_cdf$cdf, inv_cdf$inv, .x)$y
        )) %>%
        unnest(x)

inv_cdf_graph <-
        inv_cdf %>%
        ggplot(aes(inv, cdf)) +
        geom_point(alpha = .2, size = 2) +
        geom_line(aes(y = cdf_simul_log),
                linewidth = 2, alpha = .3,
                color = "gray70"
        ) +
        geom_point(
                data = mid_point,
                aes(x, y), shape = 3,
                size = 15, color = "gray50"
        ) + 
        scale_y_continuous(
                        limits = c(0,1),
                        breaks = c(0, seq(.1, .9, .2), 1),
                        labels = scales::label_percent()) +
        scale_x_continuous(breaks = seq(0, maxventory, 5)) +
        labs(x = "Inventory", y = "",
             title = "Cumulative density function of homes in inventory at any given moment",
             caption = "Based on sales from 2018 and later") +
        theme(
                panel.grid.minor = element_blank(),
                plot.caption.position = "plot",
                plot.caption = element_text(hjust = 0),
                plot.title.position = "plot"
        )

ggsave("graphs/homeinventory_cdf.png", width = 7, height = 5, plot = inv_cdf_graph)
