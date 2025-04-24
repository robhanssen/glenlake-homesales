library(tidyverse)
library(lubridate)
theme_set(
    theme_light() +
        theme(
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0),
            panel.grid.minor = element_blank()
        )
)

load("Rdata/homesales.Rdata")

homesales %>%
    drop_na(amount) %>%
    summarize(
        min_price = min(amount),
        max_price = max(amount),
        median_price = median(amount),
        mean_price = mean(amount),
        quant25 = quantile(amount, .25),
        quant75 = quantile(amount, .75),
        .by = saleyear
    ) %>%
    ggplot(
        aes(x = saleyear)
    ) +
    geom_errorbar(
        aes(ymin = min_price, ymax = max_price),
        width = .25
    ) +
        geom_errorbar(
        aes(ymin = quant25, ymax = quant75),
        width = .5
    ) +
    geom_point(
        aes(y = median_price),
        shape = 10, size = 4
    ) +
    # geom_point(
    #     aes(y = mean_price),
    #     shape = 1, size = 4
    # ) +
    scale_y_continuous(
        labels = scales::label_currency(),
        breaks = 1e3 * c(150, seq(200, 800, 100))
    ) +
    labs(
        x = NULL,
        y = "Sale price (in US$)",
        title = "Price development over the last years",
        caption = "Errorbars show absolute minimum and maximum sale price. Points indicate the median and mean sale price"
    ) + 
    coord_flip()

homesales %>%
    drop_na(amount) %>%
    ggplot(aes(y = factor(saleyear), x = amount)) +
    ggridges::geom_density_ridges() +
    scale_x_continuous(
        labels = scales::label_currency(),
        breaks = 1e3 * c(150, seq(200, 800, 100))
    )
