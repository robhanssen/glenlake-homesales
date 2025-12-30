library(tidyverse)

load("Rdata/homesales.Rdata")

tdata <-
    homesales %>%
    drop_na(saledate) %>%
    arrange(saledate) %>%
    mutate(
        salemonth = floor_date(saledate, unit = "month"),
        x = 1,
    ) %>%
    reframe(
        sales = sum(x),
        .by = salemonth
    )


times <- tibble(salemonth = seq(
    floor_date(min(tdata$salemonth), unit = "month"),
    floor_date(max(tdata$salemonth), unit = "month"),
    by = "1 month"
))

timeseries <- full_join(times, tdata, by = "salemonth") %>%
    replace_na(list(sales = 0)) %>%
    mutate(
        saleyear = year(salemonth)
    ) %>%
    mutate(
        yearsales = as.integer(cumsum(sales)),
        .by = saleyear
    ) %>%
    filter(saleyear > 2017)


sales_ts <-
    ts(
        timeseries$yearsales,
        start = c(year(first(timeseries$salemonth)), month(first(timeseries$salemonth))),
        frequency = 12
    )
data_f <- forecast::forecast(sales_ts, h = 23)

plot(data_f)