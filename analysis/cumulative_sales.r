library(tidyverse)
library(lubridate)
theme_set(theme_minimal())

load("Rdata/homesales.Rdata")


sale_turnover <-
    homesales %>%
    filter(!is.na(saledate)) %>%
    arrange(saledate) %>%
    mutate(
        y = 1,
        sold = cumsum(y),
        turnover = sold / 482
    ) %>%
    rename(date = saledate)

listing_turnover <-
    homesales %>%
    # filter(!is.na(saledate)) %>%
    arrange(listingdate) %>%
    mutate(
        y = 1,
        listed = cumsum(y),
        turnover = listed / 482
    ) %>%
    rename(date = listingdate)

period <-
    tribble(
        ~from, ~to, ~name,
        20190601, 20220101, "period 1",
        20230401, 20250301, "period 2"
    ) %>%
    mutate(across(1:2, ymd))


seq_len(2)

fit <- 
    map(seq_len(nrow(period)), \(p) {
    to <- period$to[p]
    from <- period$from[p]
    name <- period$name[p]
    sale_turnover %>% filter(date < to & date >= from)
}) %>%
    map(\(dat) {
        with(dat, lm(turnover ~ date))
    }) %>%
    map_df(broom::augment) #%>%
    # mutate(estimate = case_when(
    #     term == "date" ~ estimate * 365 * 482,
    #     TRUE ~ estimate
    # ))



ggplot(listing_turnover) +
    aes(x = date, y = turnover) +
    # geom_line(show.legend = FALSE, color = "dodgerblue") +
    geom_line(data = sale_turnover, color = "red") +
    scale_y_continuous(labels = scales::percent_format()) + 
    geom_line(data = fit, aes(y = .fitted))
