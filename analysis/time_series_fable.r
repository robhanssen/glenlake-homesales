library(tidyverse)
library(lubridate)
library(fable)
library(tsibble)
library(tsibbledata)

load("Rdata/homesales.Rdata")


model_years = 2

timedata <-
    homesales %>%
    mutate(ymth = yearmonth(saledate)) %>%
    group_by(ymth) %>%
    summarize(amount_mean = mean(amount, na.rm = TRUE),
            amount_median = median (amount, na.rm = TRUE),
            .groups = "drop") %>%
    drop_na(ymth) %>%
    as_tsibble(.) %>%
    tsibble::fill_gaps()




timedata %>%
    model(
        ets = ETS(box_cox(amount_median, 0.3)),
        arima = ARIMA(amount_median),
        snaive = SNAIVE(amount_median)
    ) %>%
    forecast(h = glue::glue("{model_years} years")) %>% 
    autoplot(timedata, level = NULL) + 
    scale_y_continuous(limits = c(0, NA),
                    breaks = seq(0, 5e6, 2e5),
                    labels = scales::dollar_format(scale = 1e-3, suffix = "K"))