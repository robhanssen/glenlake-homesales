library(tidyverse)
library(patchwork)
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


average_by_period_graph <- function(dat, var, period = 52, probs = c(0.10, 0.90), plot = "av") {
    first_date <- with(dat, min(c(saledate, listingdate), na.rm = TRUE))
    last_date <- today()
    # time_window <- period # weeks

    date_seq <- seq(first_date + weeks(period), last_date, by = "month")

    av <-
        map_df(
            date_seq,
            ~ tibble(
                date = .x,
                var_list =
                    list(
                        dat %>%
                            filter(saledate < .x, saledate >= .x - weeks(period)) %>%
                            pull({{ var }})
                    )
            )
        ) %>%
        mutate(
            av = map_dbl(var_list, mean),
            md = map_dbl(var_list, median),
            qlow = map_dbl(var_list, quantile, prob = min(probs)),
            qhigh = map_dbl(var_list, quantile, prob = max(probs)),
            nsale = map_dbl(var_list, length)
        )

    last_av <-
        first(av) %>%
        select(date, av, qlow, qhigh) %>%
        pivot_longer(!date) %>%
        mutate(text = c(
            "Average",
            glue::glue("{scales::percent(probs[1], suffix = ' %')}tile"),
            glue::glue("{scales::percent(probs[2], suffix = ' %')}tile")
        ))

    if (plot == "av") {
        av %>%
            ggplot(
                aes(x = date, y = av)
            ) +
            geom_line() +
            geom_line(aes(y = qlow), linewidth = 2, color = "gray50", alpha = .3) +
            geom_line(aes(y = qhigh), linewidth = 2, color = "gray50", alpha = .3) +
            scale_x_date(
                date_labels = "%Y",
                # date_breaks = "1 year"
            ) +
            geom_text(
                data = last_av,
                aes(x = date, y = value * c(1, .9, 1.1), label = text),
                inherit.aes = FALSE, hjust = 0
            )
    } else if (plot == "n") {
        av %>%
            ggplot(
                aes(x = date, y = nsale / 482)
            ) +
            geom_line() +
            scale_x_date(
                date_labels = "%Y"
            )
    }
}

time_window <- 52

amount_g <-
    average_by_period_graph(homesales, amount, period = time_window) +
    labs(
        x = "", y = "Average sales price (in $)",
        title = glue::glue("Average home sales price in the previous {time_window} weeks")
    ) +
    scale_y_continuous(
        limits = c(0, NA),
        breaks = seq(0, 500, 100) * 1e3,
        labels = scales::label_dollar()
    )

time_g <-
    average_by_period_graph(homesales, timeonmarket) +
    labs(
        x = "", y = "Average time on market (in day)",
        title = glue::glue("Average time on market in the previous {time_window} weeks")
    ) +
    scale_y_continuous(
        limits = c(0, NA),
        breaks = seq(0, 500, 50),
    )

number_g <-
    average_by_period_graph(homesales, timeonmarket, plot = "n") +
    labs(
        x = "", y = "Homes sold (%)",
        title = glue::glue("Fraction of homes sold in the previous {time_window} weeks")
    ) +
    scale_y_continuous(
        labels = scales::label_percent(),
        limits = c(0, NA),
        # breaks = seq(0, 1, .20),
    )


ggsave(
    "graphs/average_by_period.png",
    width = 10, height = 10,
    plot = (amount_g + time_g)/(number_g + plot_spacer()) +
        plot_annotation(
            caption = "Based on home sales as of 2017 in Glen Lake"
        )
)
