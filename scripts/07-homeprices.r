library(tidyverse)
library(lubridate)
theme_set(theme_light())

load("Rdata/homesales.Rdata")

this_year <- year(today())

lowest_amount <- with(
    homesales,
    median(amount[saleyear == 2018 & hometype != "townhome"],
        na.rm = TRUE
    )
)


amount_thisyear <- with(
    homesales,
    median(amount[saleyear == this_year & hometype != "townhome"],
        na.rm = TRUE
    )
)

# 5-color set from ColorBrewers
# this will eventually fail when there are more than 120 year in the dataset (i.e. never)
# should this happen, increase the "10" to a larger number
colors <- rep(RColorBrewer::brewer.pal(12, "Paired"), 10)

am_factor <- amount_thisyear / lowest_amount
prefix <- ifelse(am_factor > 1, "+","")
chc <- scales::percent(am_factor - 1, prefix = prefix)

homesales %>%
    filter(!is.na(saledate), hometype != "townhome", saleyear > 2017) %>%
    mutate(year = factor(saleyear)) %>%
    ggplot() +
    aes(x = amount, y = year, fill = year, color = year) +
    geom_vline(
        xintercept = c(lowest_amount, amount_thisyear),
        lty = 1, linewidth = 2, alpha = .2
    ) +
    ggridges::geom_density_ridges2(show.legend = FALSE, alpha = .4) +
    scale_x_continuous(
        labels = scales::dollar_format(),
        sec.axis = sec_axis(~ . / lowest_amount - 1,
            labels = scales::percent_format(),
        ),
        breaks = 1e5 * 0:10
    ) +
    labs(
        x = "Home sale price", y = NULL,
        title = "Changes in home sale prices in Glen Lake",
    ) +
    annotate("text",
        x = lowest_amount - 5e3, y = .75,
        label = "Median value\n2018", hjust = 1
    ) +
    annotate("text",
        x = amount_thisyear + 5e3, y = .75,
        label = glue::glue("Median value\n{this_year} ({chc})"), hjust = 0
    ) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors)

ggsave("graphs/homeprice-change.png", width = 8, height = 7)


tax_years <- seq(1998, 2200, 5)
last_tax_year <- max(tax_years[which(year(today()) > tax_years)])

lowest_amount <- with(
    homesales,
    median(amount[saleyear == last_tax_year & hometype != "townhome"],
        na.rm = TRUE
    )
)

homesales2018 <-
    homesales %>%
    filter(saleyear == last_tax_year) %>%
    mutate(saleyear = last_tax_year - 0.5)

am_factor <- amount_thisyear / lowest_amount
prefix <- ifelse(am_factor > 1, "+","")
chc <- scales::percent(am_factor - 1, prefix = prefix)

homesales %>%
    filter(!is.na(saledate), hometype != "townhome", saleyear > 2017) %>%
    filter(saleyear %in% c(last_tax_year, this_year)) %>%
    bind_rows(homesales2018) %>%
    mutate(year = factor(saleyear)) %>%
    mutate(amount = case_when(
        year == last_tax_year ~ amount * am_factor,
        TRUE ~ amount
    )) %>%
    mutate(dyear = case_when(
        year == last_tax_year - 0.5 ~ "2023\n(actual)",
        year == last_tax_year ~ "2023\n(extrapolated)",
        TRUE ~ paste(year, "\n(actual)")
    )) %>%
    ggplot() +
    aes(x = amount, y = dyear, fill = year, color = year) +
    geom_vline(
        xintercept = c(lowest_amount, amount_thisyear),
        lty = 1, linewidth = 2, alpha = .2
    ) +
    ggridges::geom_density_ridges2(show.legend = FALSE, alpha = .4) +
    scale_x_continuous(
        labels = scales::dollar_format(),
        sec.axis = sec_axis(~ . / lowest_amount - 1,
            labels = scales::percent_format(),
        ),
        breaks = 1e5 * 0:10
    ) +
    labs(
        x = "Home sale price", y = NULL,
        title = "Changes in home sale prices in Glen Lake",
        subtitle = glue::glue("Prices in {last_tax_year} were extrapolated to {this_year} by changing {chc}") # nolint
    ) +
    annotate("text",
        x = lowest_amount - 5e3, y = .75,
        label = glue::glue("Median value\n{last_tax_year}"), hjust = 1
    ) +
    annotate("text",
        x = amount_thisyear + 5e3, y = 1.25,
        label = glue::glue("Median value\n{this_year} ({chc})"), hjust = 0
    ) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors)

ggsave("graphs/homeprice-extrapolation.png", width = 8, height = 7)
