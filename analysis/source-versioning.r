# source-versioning.r

library(readr)
library(magrittr)

load("Rdata/homesales.Rdata")

years <- with(homesales,
    unique(
        c(listingyear[!is.na(listingyear)],
          saleyear[!is.na(saleyear)])
    )
)

nothing <- purrr::map_df(
    years,
    ~ homesales %>%
        filter(listingyear == .x) %>%
        write_csv(paste0("tmp/listings", .x, ".csv"))
)