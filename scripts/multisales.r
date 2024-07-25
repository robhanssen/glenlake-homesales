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

count_g <-
    homesales %>%
    count(address, hometype) %>%
    filter(n > 1) %>%
    mutate(street = str_trim(str_remove_all(address, "\\d"))) %>%
    mutate(address = fct_reorder(address, n)) %>%
    ggplot(
        aes(y = address, x = n, fill = hometype)
    ) + 
    geom_col()

ggsave(
    "graphs/multisale-count.png",
    height = 8, width = 4,
    plot = count_g
)