library(tidyverse)
library(lubridate)
library(patchwork)
source("functions/config.r")
theme_set(theme_light())

load("Rdata/homesales.Rdata")


phases <-
    read_csv("sources/glenlakehomes-phases.csv") %>%
    mutate(location = case_when(
        phase %in% c(2) ~ "back",
        TRUE ~ "front"
    ))

homesales_joined <-
    homesales %>%
    filter(!is.na(saledate)) %>%
    mutate(streetname = substr(address, 5, 100)) %>%
    left_join(phases, by = "streetname")

phases_size <-
    phases %>%
    group_by(location) %>%
    summarize(homes = sum(numberofhomes), .groups = "drop")

homes_sold_last_12months <- function(tbl, date) {
    date_year_ago <- date - lubridate::years(1)
    sale_data <-
        tbl %>%
        filter(saledate > date_year_ago, saledate <= date)

    tibble::tibble(date = date, homesold = nrow(sale_data))
}

datelist <-
    with(
        homesales_joined,
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

phaselist <- sort(unique(phases$location))

if (exists("final")) rm(final)

for (ph in phaselist) {
    phasesize <- phases_size$homes[phases_size$location == ph]

    homes <-
        homesales_joined %>%
        filter(location == ph)
    x <-
        map_dfr(monthlist, ~ homes_sold_last_12months(homes, .x)) %>%
        mutate(phase = ph) %>%
        mutate(res_time = phasesize / homesold)

    if (exists("final")) final <- bind_rows(final, x) else final <- x
}


cleaned_up_final <-
    final %>%
    select(-res_time) %>%
    pivot_wider(names_from = phase, values_from = homesold) %>%
    unnest(back) %>%
    unnest(front) %>%
    mutate(total = back + front) %>%
    pivot_longer(back:total, names_to = "phase", values_to = "homesold") %>%
    mutate(phase = factor(phase, levels = c("front", "back", "total")))

colors <- c("front" = "red", "back" = "darkgreen", "total" = "gray50")
linesty <- c(2, 2, 1)

cleaned_up_final %>%
    ggplot() +
    aes(date, homesold, color = factor(phase), lty = factor(phase)) +
    geom_line() +
    scale_color_manual(values = colors) +
    scale_linetype_manual(values = linesty) +
    labs(
        x = "Date",
        y = "Homes sold in the last 12 months",
        color = "Phase",
        linestyle = NA
    ) +
    theme(legend.position = "none") +
    annotate("text", x = ymd(20210101), y = 18, label = "Back", color = "darkgreen") +
    annotate("text", x = ymd(20200101), y = 28, label = "Front", color = "red") +
    annotate("text", x = ymd(20200101), y = 55, label = "Total", color = "gray50")

phases_size_with_total <-
    bind_rows(
        phases_size,
        tibble(location = "total", homes = sum(phases_size$homes))
    )


cleaned_up_final %>%
    left_join(phases_size_with_total, by = c("phase" = "location")) %>%
    mutate(pct = homesold / homes) %>%
    ggplot() +
    aes(date, pct, color = factor(phase), lty = factor(phase)) +
    geom_line() +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_color_manual(values = colors) +
    scale_linetype_manual(values = linesty) +
    labs(
        x = "Date",
        y = "Homes sold in the last 12 months",
        color = "Phase",
        linestyle = NA
    ) +
    theme(legend.position = "none") +
    annotate("text", x = ymd(20220501), y = 0.05, label = "Back", color = "darkgreen") +
    annotate("text", x = ymd(20220501), y = .13, label = "Front", color = "red") +
    annotate("text", x = ymd(20220501), y = .08, label = "Total", color = "gray50")

ggsave("graphs/turnover-by-phase.png", width = 8, height = 6)
