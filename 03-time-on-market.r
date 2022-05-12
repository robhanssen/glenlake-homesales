message("04-time-on-market.r")
library(tidyverse)

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
        facet_wrap(hometype ~ status, ncol = 3)


ggsave("graphs/median-time-on-market.pdf")

homesales %>%
        filter(status == "Sold") %>%
        ggplot() +
        aes(x = factor(listingyear), y = timeonmarket, fill = hometype) +
        geom_violin(draw_quantiles = 0.5) +
        geom_jitter(alpha = .5, width = .2) +
        facet_wrap(. ~ hometype) +
        labs(
                x = "Year of listing",
                y = "Time on market (in days)",
                title = "Distribution of time on market for sold homes in Glen Lake",
                fill = "Home type",
                caption = caption
        ) +
        scale_y_continuous(limits = c(0, 350))

ggsave("graphs/violinplot-time-on-market.pdf", width = 11, height = 8)