library(furrr)
library(ggplot2)
library(ggridges)
theme_set(theme_light())

plan(multisession)

load("Rdata/homesales.Rdata")

period_list <- sort(c(30 * 3:6, 270, 365))

multiperiod <- function(period, homesales = homesales, niter = 1000) {

    future::plan(multisession)

    mc <- function(value, change = 0, variability = .2) {
        ch <- rnorm(1, change, variability)
        outval <- value * (1 + ch)
        round(outval, 0)
    }

    jnow <- lubridate::today()
    homesales_adjusted <- dplyr::filter(homesales, !is.na(amount))

    t <- dplyr::arrange(homesales, listingdate)
    cumulativelist <- dplyr::mutate(t, y = 1, culisting = cumsum(y))

    t <- dplyr::arrange(homesales_adjusted, saledate)
    cusales <- dplyr::mutate(t,
        y = 1,
        cusale = cumsum(y),
        cumkt = cumsum(amount)
    )


    mod_listings <- coef(
        lm(culisting ~ listingdate,
            data = dplyr::filter(
                cumulativelist,
                listingdate > jnow - lubridate::days(period)
            )
        )
    )

    slope <- as.numeric(mod_listings[2])

    freq <- 1 / slope

    median_sale <- with(
        dplyr::filter(
            homesales_adjusted,
            saledate > jnow - lubridate::days(period)
        ),
        median(amount, na.rm = TRUE)
    )

    median_time <- with(
        dplyr::filter(
            homesales_adjusted,
            saledate > jnow - lubridate::days(period)
        ),
        median(timeonmarket)
    )

    t <- dplyr::filter(
        cusales,
        saledate >= lubridate::floor_date(jnow, unit = "year")
    )
    t <- dplyr::summarize(t,
        min = min(cumkt),
        max = max(cumkt)
    )
    mkt_beginyear <- dplyr::mutate(t, growth = max - min)

    n_max <- niter
    now <- jnow
    year_end <- lubridate::ceiling_date(now, unit = "year")

    simul <- function(n) {
        totalsale <- 0
        ldate <- now

        while (ldate < year_end) {
            ldate <- ldate + lubridate::days(mc(freq, variability = .7))
            sdate <- ldate + lubridate::days(mc(median_time, variability = .7))
            if (sdate < year_end) {
                sale <- mc(median_sale, variability = .2)
                totalsale <- totalsale + sale
            }
        }
        tibble::tibble(data = totalsale + mkt_beginyear$growth)
    }

    numberlist <- furrr::future_map_dfr(1:n_max, ~simul(.x))

    dplyr::mutate(numberlist, period = period)
}

multip <- furrr::future_map_dfr(period_list, ~multiperiod(.x, homesales = homesales, niter = 1000))

ggplot(multip) +
    aes(data, factor(period), fill = factor(period)) +
    geom_density_ridges(alpha = .2) +
    scale_x_continuous(
        labels = scales::dollar_format(scale = 1e-6, suffix = " M"),
        breaks = 1e6 * 0:20
    ) +
    labs(x = "Expected market value", y = "Predictor period length (in days)") +
    theme(legend.position = "none")

ggsave("montecarlo/mc-par5.png")