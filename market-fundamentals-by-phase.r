library(tidyverse)
library(lubridate)
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
length(monthlist)
phaselist <- sort(unique(phases$location))

rm(final)

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

final %>%
    ggplot() +
    aes(date, homesold, color = factor(phase)) +
    geom_line() +
    ylim(0, 65)


p2 <- final %>%
    select(-res_time) %>%
    pivot_wider(names_from = phase, values_from = homesold) %>%
    mutate(total = back + front) %>%
    pivot_longer(back:total, names_to = "phase", values_to = "homesold") %>%
    ggplot() +
    aes(date, homesold, color = factor(phase)) +
    geom_line() + 
    ylim(0, 65)



p1 <- map_dfr(monthlist, ~ homes_sold_last_12months(homesales_joined, .x)) %>%
    ggplot() +
    aes(date, homesold) +
    geom_line() +
    ylim(0, 65)

library(patchwork)

p1 + p2