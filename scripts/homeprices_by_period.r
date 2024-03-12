library(tidyverse)
library(zoo)

theme_set(
    theme_light() +
        theme(
            plot.title.position = "plot",
            plot.title = element_text(hjust = 0),
            panel.grid.minor = element_blank(),
            legend.position = "none",
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0)
        )
)

load("Rdata/homesales.Rdata")

homesales <- homesales %>%
    replace_na(list(saledate = today())) %>%
    mutate(timeonmarket = saledate - listingdate)

first_date <- with(homesales, min(c(saledate, listingdate), na.rm = TRUE))
last_date <- today()
time_window <- 52 # weeks

probs <- c(0.10, 0.90)

date_seq <- seq(first_date + weeks(time_window), last_date, by = "month")

av_amount <-
    map_df(
        date_seq,
        ~ tibble(
            date = .x,
            amount_list =
                list(
                    homesales %>%
                        filter(saledate < .x, saledate >= .x - weeks(time_window)) %>%
                        pull(amount)
                )
        )
    ) %>%
    mutate(
        av = map_dbl(amount_list, mean),
        md = map_dbl(amount_list, median),
        qlow = map_dbl(amount_list, quantile, prob = min(probs)),
        qhigh = map_dbl(amount_list, quantile, prob = max(probs)),
        nsale = map_dbl(amount_list, length)
    )


last_av_amount <-
    first(av_amount) %>%
    mutate(qlow = .8 * qlow, qhigh = 1.2 * qhigh) %>%
    select(date, av, qlow, qhigh) %>%
    pivot_longer(!date) %>%
    mutate(text = c(
        "Average",
        glue::glue("{scales::percent(probs[1], suffix = ' %')}tile"),
        glue::glue("{scales::percent(probs[2], suffix = ' %')}tile")
    ))

amount_g <-
    av_amount %>%
    ggplot(
        aes(x = date, y = av)
    ) +
    geom_line() +
    labs(
        x = "", y = "Average sales price (in $)",
        title = glue::glue("Average home sales price in the previous {time_window} weeks")
    ) +
    geom_line(aes(y = qlow), linewidth = 2, color = "gray50", alpha = .3) +
    geom_line(aes(y = qhigh), linewidth = 2, color = "gray50", alpha = .3) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    scale_y_continuous(
        limits = c(0, NA),
        breaks = seq(0, 500, 50) * 1e3,
        labels = scales::label_dollar()
    ) +
    geom_text(
        data = last_av_time,
        aes(x = date, y = value * c(1, .9, 1.1), label = text),
        inherit.aes = FALSE, hjust = 0
    )

ggsave(
    "graphs/salesprice_by_period.png",
    width = 8, height = 5,
    plot = amount_g
)
