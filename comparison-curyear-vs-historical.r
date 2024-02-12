library(tidyverse)
library(patchwork)

theme_set(theme_light() +
    theme(
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot"
    ))

load("Rdata/homesales.Rdata")

this_year <- year(today())

years <- unique(na.exclude(homesales$saleyear))
years <- years[which(years != this_year)]

mean_curyear <-
    with(
        homesales,
        mean(amount[saleyear == this_year], na.rm = TRUE)
    )

meantime_curyear <-
    with(
        homesales,
        as.numeric(mean(timeonmarket[saleyear == this_year], na.rm = TRUE))
    )




salesprice_g <-
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
    geom_point(aes(y = estimate1 - estimate2 + mean_curyear)) +
    geom_errorbar(
        aes(
            ymin = conf.high + mean_curyear,
            ymax = conf.low + mean_curyear
        ),
        width = .3
    ) +
    scale_y_continuous(
        labels = scales::label_dollar(),
        sec.axis = sec_axis(~ . / mean_curyear - 1, labels = scales::percent_format())
    ) +
    geom_text(aes(
        x = year, y = mean_curyear + conf.high + 1e4,
        label = scales::pvalue(p.value)
    )) +
    geom_hline(yintercept = mean_curyear, linetype = "dashed") +
    labs(
        x = "", y = glue::glue("Saleprice"),
        title = glue::glue(
            "Saleprice comparison between",
            " {this_year} and other years"
        ),
    )

time_g <-
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
    geom_point(aes(y = estimate1 - estimate2 + meantime_curyear)) +
    geom_errorbar(
        aes(
            ymin = conf.high + meantime_curyear,
            ymax = conf.low + meantime_curyear
        ),
        width = .3
    ) +
    geom_text(aes(
        x = year, y = conf.high + 8 + meantime_curyear,
        label = scales::pvalue(p.value)
    )) +
    geom_hline(yintercept = 0 + meantime_curyear, linetype = "dashed") +
    labs(
        x = "", y = glue::glue("Time on market"),
        title = glue::glue(
            "Time on market comparison between",
            " {this_year} and other years"
        ),
    )

ggsave("graphs/time-price-comparison.png",
    width = 8, height = 6,
    plot = salesprice_g / time_g
)
