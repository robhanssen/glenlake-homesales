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
    arrange(listingdate) %>%
    mutate(
        salediff = listingdate - lag(listingdate)
    ) %>%
    filter(listingyear > 2017)

lastdiff <- today() - max(homesales$listingdate, na.rm = TRUE)

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
    glue::glue("A home is listed every {halflife} days in Glen Lake on average (95% CI {q95} days)")

subtitle <-
    glue::glue("The current time between listings is {lastdiff} days ({lastvalue}ile)")

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
        x = "Time between home listings (in days)",
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
    "graphs/time_between_listings_dist.png",
    width = 8, height = 5,
    plot = lag_g
)

#
# multi-year analysis
#

determine_cdf <- function(dat) {
    cdf <- ecdf(dat$salediff)
    tibble(
        x = 0:maxdiff,
        y = cdf(x)
    )
}

lagsales %>%
    filter(!is.na(salediff)) %>%
    nest(data = !listingyear) %>%
    mutate(
        cdf = map(data, ~ determine_cdf(.x)),
        mod = map(cdf, ~ nls(y ~ pexp(x, rate = lambda), start = list(lambda = .1), data = .x)),
        pars = map(mod, broom::tidy),
        preddate = map(mod, broom::augment)
    ) %>%
    unnest(pars) %>%
    mutate(q95 = qexp(.95, rate = estimate)) %>%
    mutate(q50 = qexp(.50, rate = estimate)) %>%
    mutate(q05 = qexp(.05, rate = estimate)) %>%
    mutate(
        time = 1 / estimate,
        rate = ddays(1) / estimate
    ) %>%
    select(-data, -cdf, -mod, -statistic, -term) %>%
    ggplot(
        aes(x = listingyear, y = q50)
    ) +
    geom_point() +
    geom_point(aes(y = time), shape = 3) +
    geom_errorbar(
        aes(ymax = q95, ymin = q05, x = listingyear),
        width = .2
    ) +
    # geom_segment(aes(y = 0, xend = saleyear, yend = time)) +
    scale_y_continuous(
        limits = c(0, NA)
    ) +
    labs(
        x = "Year",
        y = "Modeled time between events (in days)",
        caption = "Higher is better"
    ) +
    coord_flip()

lagsales %>%
    filter(!is.na(salediff)) %>%
    nest(data = !listingyear) %>%
    mutate(
        cdf = map(data, ~ determine_cdf(.x)),
        mod = map(cdf, ~ nls(y ~ pexp(x, rate = lambda), start = list(lambda = .1), data = .x)),
        pars = map(mod, broom::tidy),
        preddate = map(mod, broom::augment)
    ) %>%
    unnest(preddate) %>%
    ggplot(aes(x, .fitted, color = factor(listingyear))) +
    geom_line(show.legend = FALSE) +
    geom_line(
        data = fitted,
        aes(x, .fitted),
        inherit.aes = FALSE,
        linetype = 2
    ) +
    facet_wrap(vars(listingyear)) +
    scale_y_continuous(
        labels = scales::label_percent(),
        breaks = seq(0, 1, .2)
    ) +
    labs(
        x = "Time between listings (in days)",
        y = "Cumulative Distribution",
        color = "Year",
        caption = "Dotted line is long-term average"
    )
