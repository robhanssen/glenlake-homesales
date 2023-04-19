library(tidyverse)
library(lubridate)
library(scales)
theme_set(theme_light())
source("functions/config.r")
load("Rdata/homesales.Rdata")

glenlakehomes <- read_csv("sources/glenlakehomes.csv")
hometypes <- read_csv("sources/hometypes.csv")

totalhomes <- with(glenlakehomes, sum(numberofhomes))
totalsold <- nrow(homesales)
averageturnover <- totalsold / totalhomes

pdf_lognorm <- function(x, mu, sig) {
       .5 * (1 + pracma::erf((log(x) - mu) / (sqrt(2) * sig)))
}

resales <-
       homesales %>%
       arrange(address, listingdate) %>%
       group_by(address) %>%
       mutate(lagtime = (listingdate - lag(saledate)) / dmonths(1)) %>%
       drop_na(lagtime) %>%
       mutate(address_year = glue::glue("{address} ({listingyear})"))

resales %>%
       ggplot() +
       aes(
              y = fct_reorder(address_year, -lagtime),
              x = lagtime,
              fill = hometype
       ) +
       geom_col() +
       labs(
              x = "Time between sale and next listing (in months)",
              y = "Address",
              fill = "Type of home", caption = caption
       ) +
       scale_x_continuous(breaks = 12 * 1:10) +
       theme(legend.position = c(.8, .8))

ggsave("graphs/turnover-time.png", width = 8, height = 6)

resales %>%
       group_by(address) %>%
       summarize(
              resales = n() + 1,
              .groups = "drop"
       ) %>%
       slice_max(resales, n = 10) %>%
       ggplot() +
       aes(
              fct_reorder(address, resales),
              resales - 1
       ) +
       geom_col() +
       scale_y_continuous(breaks = 0:100) +
       labs(
              x = "Address",
              y = "Resale count"
       ) +
       coord_flip()

minsaleyear <- with(homesales, min(saleyear, na.rm = TRUE))
maxsaleyear <- with(homesales, max(saleyear, na.rm = TRUE))

data_time_length <- with(
       homesales,
       time_length(
              difftime(
                     max(listingdate, na.rm = TRUE),
                     min(listingdate, na.rm = TRUE)
              ),
              unit = "year"
       )
)

homesales %>%
       inner_join(hometypes) %>%
       mutate(
              streetname = substr(address, 5, 100),
              streetname = ifelse(is.na(hometypeAB) |
                     streetname == "Grays Harbor",
              streetname,
              paste(streetname, hometypeAB)
              )
       ) %>%
       group_by(streetname) %>%
       summarize(
              countbystreet = n(),
              .groups = "drop"
       ) %>%
       right_join(glenlakehomes) %>%
       mutate(countbystreet = ifelse(is.na(countbystreet),
              0,
              countbystreet
       )) %>%
       mutate(
              turnover = countbystreet / numberofhomes,
              torate = turnover / data_time_length,
              residencetime = data_time_length / turnover
       ) %>%
       mutate(homecount = cut(numberofhomes,
              breaks = c(0, 10, 20, 100),
              labels = c(
                     "10 homes or less per street",
                     "11-20 homes per street",
                     "over 20 homes per street"
              )
       )) -> hometurnover

turnoversd <- with(hometurnover, sd(turnover))
turnoverlimits <-
       averageturnover +
       c(-1, 1) * turnoversd * qnorm(0.975) / sqrt(nrow(hometurnover))

hometurnover %>%
       mutate(turnoverwarning = cut(turnover,
              c(
                     0,
                     turnoverlimits[1],
                     turnoverlimits[2],
                     turnoverlimits[2] * 2,
                     1000
              ),
              label = c("low", "ave", "higher", "high")
       )) -> hometurnover

colorset <- c(
       "low" = "green",
       "ave" = "gold",
       "higher" = "orange",
       "high" = "red"
)


caption <- paste0(
       caption_source,
       "\nDotted line: neigborhood average (",
       round(100 * turnoverlimits[1], 0),
       "-",
       round(100 * turnoverlimits[2], 0),
       "%)",
       "\n PH: patio home; TH: townhome"
)

hometurnover %>%
       ggplot() +
       aes(
              x = fct_reorder(streetname, turnover),
              y = turnover,
              fill = turnoverwarning
       ) +
       scale_y_continuous(labels = percent_format(), breaks = .25 * 1:10) +
       geom_col() +
       facet_wrap(~homecount, scale = "free_y") +
       ggtitle("Home turn-over by street") +
       labs(
              x = "Street", y = paste0(
                     "Turn-over rate (",
                     minsaleyear, "-",
                     maxsaleyear, ")"
              ),
              caption = caption
       ) +
       geom_hline(yintercept = turnoverlimits, lty = 2, color = "gray50") +
       scale_fill_manual(values = colorset) +
       coord_flip() +
       theme_light() +
       theme(legend.position = "none")


max_time <- with(
       homesales,
       (max(saledate, na.rm = TRUE) - min(saledate, na.rm = TRUE)) / dyears(1)
)
homessold <- nrow(homesales %>% filter(!is.na(saledate)))

real_average <- (homessold / 482) / max_time
real_restime <- 1 / real_average


hometurnover %>%
       ggplot() +
       aes(
              x = fct_reorder(streetname, residencetime),
              y = residencetime,
              fill = turnoverwarning
       ) +
       geom_col() +
       facet_wrap(~homecount, scale = "free_y") +
       ggtitle("Residence time by street") +
       labs(x = "Street", y = "Average residence time (in years)") +
       scale_fill_manual(values = colorset) +
       geom_hline(
              yintercept = real_restime,
              lty = 2,
              color = "gray50"
       ) +
       coord_flip() +
       theme_light() +
       theme(legend.position = "none")

ggsave("graphs/turnover-residencetime.png", width = 12, height = 6)


caption <- paste0(
       caption_source,
       "\nDotted line: neigborhood average (",
       round(100 * turnoverlimits[1] / data_time_length, 0),
       "-",
       round(100 * turnoverlimits[2] / data_time_length, 0),
       "%)",
       "\n PH: patio home; TH: townhome"
)

hometurnover %>%
       ggplot() +
       aes(
              x = fct_reorder(streetname, turnover),
              y = torate,
              fill = turnoverwarning
       ) +
       scale_y_continuous(labels = percent_format(), breaks = .05 * 0:10) +
       geom_col() +
       facet_wrap(~homecount, scale = "free") +
       ggtitle("Annual home turn-over rate by street") +
       labs(
              x = "Street", y = paste0(
                     "Turn-over rate per year (",
                     minsaleyear, "-",
                     maxsaleyear, ")"
              ),
              caption = caption
       ) +
       geom_hline(
              yintercept = turnoverlimits / data_time_length,
              lty = 2,
              color = "gray50"
       ) +
       geom_hline(
              yintercept = real_average,
              lty = 2,
              color = "gray50"
       ) +
       scale_fill_manual(values = colorset) +
       coord_flip() +
       theme_light() +
       theme(legend.position = "none")

ggsave("graphs/turnover-rate-by-street.png", width = 12, height = 6)


#
# CDF
#
resales <-
       homesales %>%
       arrange(address, listingdate) %>%
       group_by(address) %>%
       mutate(lagtime = (saledate - lag(saledate)) / dmonths(1)) %>%
       ungroup() %>%
       drop_na(lagtime) %>%
       mutate(address_year = glue::glue("{address} ({listingyear})"))

sorted_resales <-
       resales %>%
       arrange(lagtime)

total_resales <- nrow(sorted_resales)
max_lag <- (max(resales$lagtime) %/% 25 + 1) * 25

logparam <-
       with(
              resales,
              list(
                     mean = mean(log(lagtime)),
                     sd = sd(log(lagtime))
              )
       )

resales_cdf <-
       map_dfr(1:max_lag, ~ tibble(
              time = .x,
              cdf = nrow(resales %>% filter(lagtime < .x)) / total_resales,
              cdf_simul = pdf_lognorm(.x, logparam[["mean"]], logparam[["sd"]])
       ))

resales_halftime <- with(
       resales_cdf,
       approx(cdf, time,
              xout = .5,
              na.rm = TRUE
       )
) %>% as_tibble()

timespan <- ceiling((today() - min(homesales$listingdate, na.rm = TRUE)) / dmonths(1))


resale_graph <-
       resales_cdf %>%
       ggplot(aes(time, cdf)) +
       geom_point(alpha = .5, shape = 1) +
       geom_point(data = resales_halftime, aes(y, x), shape = 3, size = 15) +
       geom_line(aes(y = cdf_simul), alpha = .2, linewidth = 2) +
       scale_y_continuous(labels = scales::label_percent()) +
       labs(
              x = "Resale time (in months)", y = "",
              title = glue::glue(
                     "The expected resale time for homes ",
                     "is {round(resales_halftime$y)} months"),
              caption = glue::glue("Based on a dataset comprising {timespan} months of sale data")
       ) +
       theme(plot.title.position = "plot")

ggsave("graphs/resale_cdf.png",
       width = 6, height = 4,
       plot = resale_graph
)
