library(tidyverse)

theme_set(theme_light())

load("Rdata/homesales.Rdata")

this_year <- year(today())

years <- unique(na.exclude(homesales$saleyear))
years <- years[which(years != this_year)]


ttest <-
    homesales %>%
    mutate(cur_year = ifelse(saleyear == this_year,
        "thisyear", "historical"
    )) %>%
    drop_na(cur_year) %>%
    t.test(amount ~ cur_year, alternative = "two.sided", data = .) %>%
    broom::tidy()


p_dist <-
    homesales %>%
    mutate(cur_year = ifelse(saleyear == this_year, saleyear, "historical")) %>%
    drop_na(cur_year) %>%
    group_by(cur_year) %>%
    ggplot(aes(x = amount, y = cur_year)) +
    geom_jitter() +
    ggridges::geom_density_ridges()



homesales %>%
    mutate(cur_year = ifelse(saleyear == this_year,
        "thisyear", "historical"
    )) %>%
    drop_na(cur_year) %>%
    t.test(timeonmarket ~ cur_year, alternative = "two.sided", data = .)


map_df(
    years,
    ~ bind_cols(tibble(year = .x), homesales %>%
        filter(saleyear %in% c(.x, this_year)) %>%
        mutate(cur_year = ifelse(saleyear == this_year,
            "thisyear", saleyear
        )) %>%
        t.test(amount ~ cur_year, data = .) %>%
        broom::tidy())
) %>%
    ggplot(aes(x = year)) +
    geom_point(aes(y = estimate1 - estimate2)) +
    geom_errorbar(aes(ymin = conf.high, ymax = conf.low), width = .3) +
    scale_y_continuous(labels = scales::label_dollar()) +
    geom_text(aes(
        x = year, y = conf.high + 1e4,
        label = scales::pvalue(p.value)
    )) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
        x = "", y = glue::glue("Difference in saleprice"),
        title = glue::glue(
            "Difference in saleprice between",
            " indicated year and {this_year}"
        ),
        subtitle = "t.test p-value shows significance"
    )

map_df(
    years,
    ~ bind_cols(tibble(year = .x), homesales %>%
        filter(saleyear %in% c(.x, this_year)) %>%
        mutate(
            cur_year = ifelse(saleyear == this_year, "thisyear", saleyear),
            timeonmarket = as.numeric(timeonmarket)
        ) %>%
        t.test(timeonmarket ~ cur_year, data = .) %>%
        broom::tidy())
) %>%
    ggplot(aes(x = year)) +
    geom_point(aes(y = estimate1 - estimate2)) +
    geom_errorbar(aes(ymin = conf.high, ymax = conf.low), width = .3) +
    geom_text(aes(
        x = year, y = conf.high + 5,
        label = scales::pvalue(p.value)
    )) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
        x = "", y = glue::glue("Difference in time on market"),
        title = glue::glue(
            "Difference in time on market between",
            " indicated year and {this_year}"
        ),
        subtitle = "t.test p-value shows significance"
    )
