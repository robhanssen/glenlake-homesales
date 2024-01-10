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

lagsales <-
    homesales %>%
    arrange(saledate) %>%
    mutate(
        salediff = saledate - lag(saledate)
    ) %>%
    filter(saleyear > 2017)

lastdiff <- today() - max(homesales$saledate, na.rm = TRUE)

maxdiff <- 10 * (as.numeric(max(c(lagsales$salediff, lastdiff))) %/% 10 + 1)

e <- ecdf(lagsales$salediff)

cddata <- tibble(
    x = seq(0, maxdiff, 1),
    y = e(x),
    t = y / max(y, na.rm = TRUE)
)

fittedmodel <-
    nls(t ~ 1 - exp(-lamda * x),
        start = list(lamda = 1),
        data = cddata
    )

fitted <-
    fittedmodel %>% broom::augment()

halflife <-
    round(qexp(.5, rate = broom::tidy(fittedmodel)$estimate))

q95 <-
    round(qexp(.95, rate = broom::tidy(fittedmodel)$estimate))

lastvalue <-
    scales::percent(pexp(as.numeric(lastdiff), rate = broom::tidy(fittedmodel)$estimate), accuracy = 0.01)

title <-
    glue::glue("A home is sold every {halflife} days in Glen Lake on average (95% CI {q95} days)")

subtitle <-
    glue::glue("The current time between sales is {lastdiff} days ({lastvalue}ile)")

lag_g <-
    lagsales %>%
    ggplot(aes(x = salediff)) +
    geom_histogram(
        aes(y = after_stat(ncount)),
        binwidth = 5,
        fill = "gray50", alpha = .5,
        # color = "white"
    ) +
    geom_vline(
        xintercept = lastdiff, color = "gray50",
        linetype = 1, linewidth = 2, alpha = .2
    ) +
    geom_line(
        data = cddata,
        aes(x, t),
        linetype = 2
    ) +
    scale_x_continuous(
        breaks = seq(0, maxdiff, 10)
    ) +
    scale_y_continuous(
        labels = scales::label_percent()
    ) +
    labs(
        x = "Time between home sales (in days)",
        y = "",
        title = title,
        subtitle = subtitle,
        caption = "Source: homesales since Jan 1, 2018"
    ) +
    geom_line(
        data = tibble(x = 0:maxdiff, y = pexp(x, rate = broom::tidy(fittedmodel)$estimate)),
        aes(x, y),
        lty = 1, alpha = .1, linewidth = 1.5
    )

ggsave(
    "graphs/time_between_sales_dist.png",
    width = 8, height = 5,
    plot = lag_g
)
