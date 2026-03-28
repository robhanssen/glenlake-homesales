library(tidyverse)
library(forecast)

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

fit <- stl(sales_ts, s.window = "period")
plot(fit)

x <- ets(sales_ts)
forecast(x, h = 12)

y <- auto.arima(sales_ts)
yy <- forecast(y, h = 12)
plot(yy)

# additional plots
monthplot(sales_ts)
