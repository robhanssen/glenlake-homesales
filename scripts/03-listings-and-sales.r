library(tidyverse)
library(lubridate)
theme_set(theme_light())

load("Rdata/homesales.Rdata")
source("functions/config.r")
source("functions/graphics.r")
# listed per year
yearlistingoverview <-
    homesales %>%
    group_by(listingyear, hometype, status) %>%
    summarise(
        listedtotal = n(),
        .groups = "drop"
    )

years_range <- with(
    yearlistingoverview,
    seq(min(listingyear), max(listingyear))
)

x_breaks <- x_breaks <- make_x_breaks(years_range, max_length = 4)

yearlistingoverview %>%
    ggplot() +
    aes(x = listingyear, y = listedtotal, fill = status) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(. ~ hometype) +
    labs(
        x = "Year of listing",
        y = "Homes listed",
        caption = caption,
        title = "Number of homes listed in Glen Lake",
        fill = "Sales status"
    ) +
    scale_x_continuous(breaks = x_breaks) +
    geom_text(aes(label = listedtotal),
        position = position_dodge(width = 0.9),
        vjust = -1
    )

ggsave("graphs/year-overview-listings.png", width = 8, height = 6)

# sold per year
yearsalesoverview <-
    homesales %>%
    group_by(saleyear, hometype, status) %>%
    summarise(
        soldtotal = n(),
        .groups = "drop"
    )

years_range <- with(
    yearsalesoverview,
    seq(min(saleyear, na.rm = TRUE), max(saleyear, na.rm = TRUE))
)

x_breaks <- x_breaks <- make_x_breaks(years_range, max_length = 4)

yearsalesoverview %>%
    filter(!is.na(saleyear)) %>%
    ggplot() +
    aes(x = saleyear, y = soldtotal, fill = hometype) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(. ~ hometype) +
    labs(
        x = "Year of sale",
        y = "Homes sold",
        caption = caption,
        title = "Number of homes sold in Glen Lake",
        fill = "Home type"
    ) +
    scale_x_continuous(breaks = x_breaks) +
    geom_text(aes(label = soldtotal),
        position = position_dodge(width = 0.9),
        vjust = -1
    )

ggsave("graphs/year-overview-sales.png", width = 8, height = 6)


# turn-over rate per year by hometype
soldhomes <-
    homesales %>%
    filter(!is.na(saledate)) %>%
    group_by(saleyear, hometype) %>%
    summarise(
        soldhomes = n(),
        .groups = "drop"
    ) %>%
    mutate(percent = case_when(
        hometype == "residential" ~ soldhomes / n_residential,
        hometype == "townhome" ~ soldhomes / n_townhomes,
        hometype == "patio home" ~ soldhomes / n_patiohomes,
        TRUE ~ 0
    ))

years_range <- with(
    soldhomes,
    seq(min(saleyear, na.rm = TRUE), max(saleyear, na.rm = TRUE))
)

x_breaks <- make_x_breaks(years_range, max_length = 10)

soldhomes %>%
    ggplot() +
    aes(x = saleyear, y = percent, fill = hometype) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
        x = "Year of listing",
        y = "Turn-over rate (in %)",
        title = "Turn-over rate in Glen Lake",
        fill = "Hometype",
        caption = caption
    ) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(labels = scales::percent_format()) +
    geom_text(
        aes(label = scales::percent(percent, accuracy = .1)),
        position = position_dodge(width = 0.9), vjust = -1
    ) +
    annotate("text", x = max_year, y = .02, label = paste(max_year, "YTD", sep = ""))

ggsave("graphs/turnover-by-hometype.png", width = 8, height = 6)

# listing counter

normal_date <- function(date, projected_year) {
    y <- lubridate::year(date)
    date + lubridate::years(projected_year - y)
}

year_range <- unique(homesales$listingyear)

year_length <- length(year_range)

color_range <- c(rep("gray50", year_length - 1), "black")
alpha_range <- c(rep(.2, year_length - 1), .9)

max_listing <-
    homesales %>%
    group_by(listingyear) %>%
    summarize(n = n()) %>%
    slice_max(n, n = 1) %>%
    mutate(scaled_n = 10 * (n %/% 10 + 1)) %>%
    pull(scaled_n)


homesales %>%
    mutate(
        display_date = normal_date(listingdate, max_year),
        y = 1
    ) %>%
    group_by(listingyear) %>%
    mutate(listingcount = cumsum(y)) %>%
    ggplot() +
    aes(x = display_date, y = listingcount, color = factor(listingyear), alpha = factor(listingyear)) +
    geom_line() +
    geom_point(data = . %>% filter(listingyear == max_year)) +
    scale_y_continuous(limit = c(0, max_listing), breaks = 10 * 0:10) +
    scale_x_date(date_break = "3 months", date_minor_breaks = "1 month", date_labels = "%b %d") +
    labs(
        x = "Date",
        y = "Cumulative number of listings per year",
        title = "Glen Lake cumulative numbers of listings by year",
        color = "Year",
        caption = caption
    ) +
    scale_color_manual(values = color_range) +
    scale_alpha_manual(values = alpha_range) +
    theme(legend.position = "none")

ggsave("graphs/dayofyear-listings.png", width = 8, height = 6)

# sale counter
max_sales <-
    homesales %>%
    group_by(saleyear) %>%
    summarize(n = n()) %>%
    slice_max(n, n = 1) %>%
    mutate(scaled_n = 10 * (n %/% 10 + 1)) %>%
    pull(scaled_n)

year_range <- unique(with(homesales, saleyear[!is.na(saleyear)]))
year_length <- length(year_range)
color_range <- c(rep("gray50", year_length - 1), "black")
alpha_range <- c(rep(.2, year_length - 1), .9)


homesales %>%
    filter(!is.na(saledate)) %>%
    arrange(saledate) %>%
    mutate(
        display_date = normal_date(saledate, max_year),
        y = 1
    ) %>%
    group_by(saleyear) %>%
    mutate(salecount = cumsum(y)) %>%
    ungroup() %>%
    ggplot() +
    aes(x = display_date, y = salecount, color = factor(saleyear), alpha = factor(saleyear)) +
    geom_line() +
    geom_point(data = . %>% filter(saleyear == max_year)) +
    scale_y_continuous(limit = c(0, max_sales), breaks = 10 * 0:10) +
    scale_x_date(
        date_break = "3 months",
        date_minor_breaks = "1 month",
        date_labels = "%b %d"
    ) +
    labs(
        x = "Date",
        y = "Cumulative number of sales per year",
        title = "Glen Lake cumulative numbers of sales by year",
        color = "Year",
        caption = caption
    ) +
    scale_color_manual(values = color_range) +
    scale_alpha_manual(values = alpha_range) +
    theme(legend.position = "none")

ggsave("graphs/dayofyear-sales.png", width = 8, height = 6)
