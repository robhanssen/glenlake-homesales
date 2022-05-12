library(tidyverse)
library(patchwork)
theme_set(theme_light())

load("Rdata/homesales.Rdata")


homesales %>% 
    slice_max(timeonmarket, n = 20) %>%
    ggplot(aes(y = fct_reorder(address, timeonmarket), x = timeonmarket)) + 
    geom_col() + 
    labs(x = "Time on market", 
        y = "Address",
        title = "Record maximum time on market") +

homesales %>%
    filter(timeonmarket != 0, !is.na(saledate)) %>%
    slice_min(timeonmarket, n = 20) %>%
    ggplot(aes(y = fct_reorder(address, -timeonmarket), x = timeonmarket)) + 
    geom_col() + 
    labs(x = "Time on market", 
        y = "Address",
        title = "Record minimum time on market") 


homesales %>%
    filter(!is.na(saledate)) %>%
    slice_max(amount, n = 20) %>%

    ggplot(aes(y = fct_reorder(interact, amount), x = amount)) + 
    scale_x_continuous(labels = scales::dollar) +

    geom_col() + 
    labs(x = "Sale price", 
        y = "Address",
        title = "Record sale price") +

homesales %>%
    filter(!is.na(saledate)) %>%
    slice_min(amount, n = 20) %>%
    mutate(interact = interaction(address, saledate)) %>%
    ggplot(aes(y = fct_reorder(interact, -amount), x = amount)) + 
    scale_x_continuous(labels = scales::dollar) +
    scale_y_discrete(labels = homesales %>% slice_min(amount, n = 20) %>% pull(address) %>% rev(.)) +
    geom_col() + 
    labs(x = "Sale price", 
        y = "Address",
        title = "Record low sale price")

homesales %>%
    ggplot(aes(x = saledate, y = timeonmarket)) + 
    geom_point() + 
    geom_smooth(method = "loess")

homesales %>%
    ggplot(aes(x = saledate, y = amount)) + 
    geom_point() + 
    geom_smooth(method = "loess") + 
    scale_y_continuous(labels = scales::dollar)


homesales %>%
    filter(!is.na(saledate)) %>%
    # group_by(address) %>%
    # summarize(amount = min(amount), .groups = "drop") %>%
    slice_max(amount, n = 10) %>%
    select(address, amount, saledate) %>% write_csv("test.csv")