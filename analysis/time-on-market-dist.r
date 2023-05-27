library(tidyverse)
theme_set(theme_classic())

load("Rdata/homesales.Rdata")

filtered_homesales <-
    homesales %>%
    filter(!is.na(saledate)) %>%
    mutate(timeonmarket = as.numeric(timeonmarket))

time_cdf <-
    tibble(
        rng = seq(1, max(filtered_homesales$timeonmarket, na.rm = TRUE), 2),
        time_cdf = map_dbl(
            rng,
            ~ nrow(filtered_homesales %>% filter(timeonmarket <= .x)) /
                nrow(filtered_homesales)
        )
    )

last10 <-
    filtered_homesales %>%
    filter(saleyear == year(today())) %>%
    select(timeonmarket) %>%
    mutate(
        perc = approx(time_cdf$rng, time_cdf$time_cdf, xout = timeonmarket)$y
    )

perc_markers <-
    tibble(perc = c(.05, .25, .5, .75, .95)) %>%
    mutate(
        timeonmarket = approx(time_cdf$time_cdf, time_cdf$rng, xout = perc)$y
    )

time_cdf_g <-
    time_cdf %>%
    ggplot(aes(rng, time_cdf)) +
    geom_vline(
        xintercept = perc_markers$timeonmarket[perc_markers$perc == .5],
        linewidth = 2, alpha = .1, color = "gray10"
    ) +
    geom_line(linewidth = 2, alpha = .3, color = "gray20") +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_log10(
        limit = c(10, NA),
        breaks = c(10, 20, 30, 50, 70, 100, 200, 300, 500, 700, 1000)
    ) +
    geom_point(data = last10, aes(x = timeonmarket, y = perc), color = "red") +
    geom_point(
        data = perc_markers, aes(x = timeonmarket, y = perc),
        color = "gray30", shape = 3, size = 5
    ) +
    labs(x = "Time on market (in days)", y = "Cumulative DensityFunction")

ggsave("graphs/time_on_market_cdf.png",
    height = 4, width = 6,
    plot = time_cdf_g
)

#
# home sale amount
# 

sale_cdf <-
    tibble(
        rng = seq(140e3, max(filtered_homesales$amount, na.rm = TRUE), 10e3),
        sale_cdf = map_dbl(
            rng,
            ~ nrow(filtered_homesales %>% filter(amount <= .x)) /
                nrow(filtered_homesales)
        )
    )

last10 <-
    filtered_homesales %>%
    filter(saleyear == year(today())) %>%
    select(amount) %>%
    mutate(
        perc = approx(sale_cdf$rng, sale_cdf$sale_cdf, xout = amount)$y
    )

perc_markers <-
    tibble(perc = c(.05, .25, .5, .75, .95)) %>%
    mutate(
        amount = approx(sale_cdf$sale_cdf, sale_cdf$rng, xout = perc)$y
    )

sale_cdf_g <-
    sale_cdf %>%
    ggplot(aes(rng, sale_cdf)) +
    geom_vline(
        xintercept = perc_markers$amount[perc_markers$perc == .5],
        linewidth = 2, alpha = .1, color = "gray10"
    ) +
    geom_line(linewidth = 2, alpha = .3, color = "gray20") +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(
            labels = scales::dollar_format()
    ) +
    geom_point(data = last10, aes(x = amount, y = perc), color = "red") +
    geom_point(
        data = perc_markers, aes(x = amount, y = perc),
        color = "gray30", shape = 3, size = 5
    ) +
    labs(x = "Sale price (in USD)", y = "Cumulative Density Function")

ggsave("graphs/sale_amount_cdf.png",
    height = 4, width = 6,
    plot = sale_cdf_g
)