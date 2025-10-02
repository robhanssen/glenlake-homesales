library(tidyverse)

options("getSymbols.warning4.0" = FALSE)


get_index <- function(index = "GDP", src = "FRED") {
    t <- quantmod::getSymbols(index, src = src, auto.assign = FALSE)
    tibble::tibble(
        date = zoo::index(t),
        index = index,
        value = as.numeric(t)
    )
}

retrieve_data <- function(indexes = c("GDP", "GDPC1"), src = "FRED") {
    purrr::map_df(indexes, ~ get_index(.x, src))
}


load("Rdata/homesales.Rdata")


cpi <- get_index("CPIAUCSL")

base_cpi <- cpi$value[cpi$date == "2018-01-01"]

homesales %>%
    full_join(cpi, by = c("saledate" = "date")) %>%
    select(address, saledate, amount, value) %>%
    arrange(saledate) %>%
    mutate(
        value_intp = zoo::na.approx(value, na.rm = FALSE),
        amount_cpi = amount * base_cpi / value_intp
    ) %>%
    drop_na(address) %>%
    ggplot(
        aes(x = saledate, y = amount_cpi)
    ) + 
    geom_point() +
    geom_smooth(method ="loess") + 
    geom_point(data = homesales, aes(y = amount), color = "red") +
    geom_smooth(data = homesales, aes(y = amount), method ="loess", color = "red") 
