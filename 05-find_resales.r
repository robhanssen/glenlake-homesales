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

dupes <- homesales %>%
       filter(duplicated(address) | duplicated(address, fromLast = TRUE)) %>%
       arrange(address) %>%
       group_by(address) %>%
       mutate(
              lagtime = listingdate - lag(saledate),
              lagtime = time_length(lagtime, "months"),
              streetname = substr(address, 5, 100)
       ) %>%
       ungroup() %>%
       select(address:hometype, lagtime, streetname)

dupes %>%
       filter(!is.na(lagtime)) %>%
       arrange(lagtime) %>%
       mutate(interact = interaction(address, listingdate)) %>%
       ggplot() +
       aes(
              x = fct_reorder(interact, -lagtime),
              y = lagtime,
              fill = hometype
       ) +
       geom_bar(stat = "identity") +
       labs(
              y = "Time between sale and next listing (in months)",
              x = "Address",
              fill = "Type of home", caption = caption
       ) +
       scale_x_discrete(labels = dupes %>%
              arrange(-lagtime) %>%
              pull(address)) +
       scale_y_continuous(breaks = 12 * 1:10) +
       coord_flip()

ggsave("graphs/turnover-time.png", width = 8, height = 6)

dupes %>%
       group_by(address) %>%
       summarize(
              resales = n(),
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
       scale_fill_manual(values = colorset) +
       coord_flip() +
       theme_light() +
       theme(legend.position = "none")

ggsave("graphs/turnover-rate-by-street.png", width = 12, height = 6)