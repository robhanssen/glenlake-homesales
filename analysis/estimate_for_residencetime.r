library(tidyverse)


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

residence <- homesales %>%  
    slice_max(saledate, by = address) %>%
    mutate(residence_time = as.numeric(today() - saledate)) %>%
    select(address, saledate, residence_time) %>%
    arrange(desc(residence_time))

f <- map_dbl(
    1:5000, 
    \(n) residence %>% filter(residence_time <= n) %>% nrow()
) / 482

plot(1:5000, f)

f2 <- head(f, 1500)

mod <- nls(f2 ~ pnorm(seq_along(f2), mean = a0, sd = a1), start = list(a0 = 1500, a1 = 1000))

a0 <- broom::tidy(mod)$estimate[1]
a1 <- broom::tidy(mod)$estimate[2]

est <- sapply(1:5000, \(t) pnorm(t, mean = a0, sd = a1))

tibble(x = 1:5000, y1 = est, y2 = f) %>%
    ggplot(
        aes(x = days(x) / years(1))
    ) + 
    geom_line(
        aes(y = y1)
    ) + 
    geom_point(
        aes(y = y2), shape = 1, alpha = .05, color = "gray50"
    )
