library(tidyverse)
library(survival)

theme_set(
    theme_light()
)

load("Rdata/homesales.Rdata")

multisale <-
    homesales %>%
    count(address) %>%
    filter(n > 1) %>%
    inner_join(homesales, by = "address", multiple = "all") %>%
    group_by(address) %>%
    mutate(
        timebetweensales = saledate - lag(saledate),
        censor = 1
    ) %>%
    drop_na(timebetweensales) %>%
    select(address, ends_with("date"), timebetweensales, censor) %>%
    ungroup() 

multisale_last <-
    multisale %>%
    slice_max(saledate, n = 1, by = address) %>%
    mutate(
        timebetweensales = today() - saledate,
        censor = 0
        ) %>%
        drop_na(timebetweensales)

onesale <-
    homesales %>%
    anti_join(multisale, by = "address") %>%
    mutate(timebetweensales = (today() - saledate)) %>%
    select(address, ends_with("date"), timebetweensales) %>%
    mutate(censor = 0) %>%
    drop_na(timebetweensales)


homesales_surv <-
    bind_rows(
        multisale, onesale, multisale_last
    ) %>%
    arrange(saledate)


wb <- survreg(Surv(timebetweensales, censor) ~ 1, data = homesales_surv)

surv <- seq(.99, .01, by = -.01)

t <- predict(wb,
    type = "quantile",
    p = 1 - surv,
    newdata = data.frame(1)
)

surv_wb <-
    data.frame(
        time = ddays(t)/dyears(1),
        surv = surv,
        upper = NA,
        lower = NA,
        std.err = NA
    )

surv_wb %>%
    ggplot(
        aes(time, surv)
    ) +
    geom_line(linetype = 2) + 
    labs(
        x = "Time living in a home",
        y = "Chance of still living in that home",
        caption = "Sales data limited, starting from Jan 2017"
    )

