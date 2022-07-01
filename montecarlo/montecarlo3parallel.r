library(doParallel)
library(ggplot2)
library(ggridges)
theme_set(theme_light())

doParallel::registerDoParallel(cores = 4)

load("Rdata/homesales.Rdata")

period_list <- sort(c(60, 30 * 3:6, 270, 365))

multiperiod <- function(period, homesales = homesales, niter = 1000) {

    library(doParallel)
    doParallel::registerDoParallel(cores = 4)

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
            ldate <- ldate + lubridate::days(mc(freq, variability = .5))
            sdate <- ldate + lubridate::days(mc(median_time, variability = .5))
            if (sdate < year_end) {
                sale <- mc(median_sale, variability = .2)
                totalsale <- totalsale + sale
            }
        }
        totalsale + mkt_beginyear$growth
    }

    numberlist <- foreach(i = 1:n_max, .combine = rbind) %dopar% simul(i)
    alist_t <- tibble::as_tibble(numberlist)
    dplyr::mutate(alist_t, period = period, data = V1)
}

multip <- 
    foreach(p = period_list, .combine = rbind) %dopar% 
        multiperiod(p, homesales = homesales, niter = 100)

ggplot(multip) +
    aes(data, factor(period), fill = factor(period)) +
    geom_density_ridges(alpha = .2) +
    scale_x_continuous(labels = scales::dollar_format())

ggsave("montecarlo/mc-par3.png")