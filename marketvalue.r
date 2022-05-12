message("marketvalue.r")
library(tidyverse)
theme_set(theme_light())

load("Rdata/homesales.Rdata")

min_sale_date <- min(homesales$saledate, na.rm = TRUE)

cumulative_sales <-
    homesales %>%
    filter(!is.na(saledate)) %>%
    arrange(saledate) %>%
    mutate(
        totalamount = cumsum(amount),
        time = as.numeric(saledate - min_sale_date)
    ) %>%
    select(time, saledate, totalamount)

salemod <- lm(totalamount ~ time + I(time^2) + I(time^3), data = cumulative_sales)

salemod %>% glance()
salemod %>% tidy()

sale_model <- salemod %>%
    augment() %>%
    mutate(saledate = time + min_sale_date)

cumulative_sales %>%
    ggplot() +
    aes(saledate, totalamount) +
    geom_point(alpha = .2) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
    geom_line(data = sale_model, aes(y = .fitted), lty = 3)