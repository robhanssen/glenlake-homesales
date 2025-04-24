library(tidyverse)
library(patchwork)

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

dat = homesales
var

time_between <- function(dat, var, labels) {
    lagsales <-
        homesales %>%
        arrange(saledate) %>%
        mutate(timeonmarket = as.numeric(timeonmarket)) %>%
        filter(year(saledate) > 2017)

    lastdiff <- max(lagsales$timeonmarket)

    maxdiff <- max(10 * (lagsales$timeonmarket %/% 10) + 10)

    e <- ecdf(lagsales$timeonmarket)

    cddata <- tibble(
        x = seq(1, maxdiff, 1),
        t = e(x),
        # t = y / max(y, na.rm = TRUE)
    )

    fittedmodel <-
        nls(t ~ pnorm(log(x), mn, sd),
            start = list(mn = log(50), sd = 1),
            data = cddata %>% filter(x > 0, t < 1)
        )

    fitted <-
        fittedmodel %>% broom::augment() %>%
        mutate(.fitted = exp(.fitted))

    # halflife <-
    #     round(qexp(1 - exp(-1), rate = broom::tidy(fittedmodel)$estimate))

    # q95 <-
    #     round(qexp(.95, rate = broom::tidy(fittedmodel)$estimate))

    lastvalue <-
        scales::percent(pexp(as.numeric(lastdiff), rate = broom::tidy(fittedmodel)$estimate), accuracy = 0.01)

    title <-
        glue::glue("A home is {labels[1]} every {halflife} days on average (95% CI {q95} days)")

    subtitle <-
        glue::glue("The current time between {labels[2]} is {lastdiff} days ({lastvalue}ile)")

    # lag_g <-
        lagsales %>%
        ggplot(aes(x = timeonmarket)) +
        geom_histogram(
            aes(y = after_stat(ncount)),
            binwidth = 5,
            fill = "gray50", alpha = .5,
            # color = "white"
        ) +
        # geom_vline(
        #     xintercept = lastdiff, color = "gray50",
        #     linetype = 1, linewidth = 2, alpha = .2
        # ) +
        geom_line(
            data = cddata,
            aes(x, t),
            linetype = 2
        ) +
        scale_x_continuous(
            # breaks = seq(0, maxdiff, 10)
        ) +
        scale_y_continuous(
            labels = scales::label_percent()
        ) +
        # labs(
        #     x = glue::glue("Time between home {labels[2]} (in days)"),
        #     y = "",
        #     title = title,
        #     subtitle = subtitle
        # ) #+
        geom_line(
            data = tibble(x = 0:maxdiff, y = pnorm(log(x), mean = broom::tidy(fittedmodel)$estimate[1], sd =  broom::tidy(fittedmodel)$estimate[2])),
            aes(x, y),
            lty = 1, alpha = .1, linewidth = 1.5
        )
}


time_between(homesales, listingdate, c("listed", "listings"))  +
time_between(homesales, saledate, c("sold", "sales")) +
plot_annotation(
    caption = "Source: homesales since Jan 1, 2018"
)

ggsave("graphs/time_between.png", width = 12, height = 8)