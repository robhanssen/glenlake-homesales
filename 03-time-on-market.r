library(tidyverse)
library(lubridate)
theme_set(theme_light())
source("functions/config.r")
load("Rdata/homesales.Rdata")

# median time on market by year and hometype
timeonmarket <- homesales %>%
        group_by(listingyear, hometype, status) %>%
        summarise(mediantimeonmarket = median(timeonmarket,
                na.rm = TRUE
        ))

timeonmarket %>%
        ggplot() +
        aes(x = listingyear, y = mediantimeonmarket, fill = hometype) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(
                x = "Year of listing",
                y = "Median time on market (in days)",
                title = "Median time on market for homes in Glen Lake",
                fill = "Home type",
                caption = caption
        ) +
        geom_text(aes(label = round(mediantimeonmarket, 0)),
                position = position_dodge(width = 0.9),
                vjust = -1
        ) +
        theme(legend.position = "none") +
        facet_grid(hometype ~ status)

ggsave("graphs/timeonmarket-median.png", width = 8, height = 6)

homesales %>%
        filter(status == "Sold") %>%
        ggplot() +
        aes(x = factor(listingyear), y = timeonmarket) +
        geom_violin(draw_quantiles = 0.5) +
        geom_jitter(alpha = .5, width = .2) +
        # facet_wrap(. ~ hometype) +
        labs(
                x = "Year of listing",
                y = "Time on market (in days)",
                title = "Distribution of time on market for sold homes in Glen Lake",
                fill = "Home type",
                caption = caption
        ) +
        scale_y_continuous(limits = c(-1, 350))

ggsave("graphs/timeonmarket-distribution.png", width = 8, height = 6)