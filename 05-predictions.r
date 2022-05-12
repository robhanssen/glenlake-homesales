message("05-predictions.r")
library(tidyverse)
library(lubridate)
library(scales)
library(broom)
source("01-config.r")

load("Rdata/homesales.Rdata")

glenlakehomes <- read_csv("sources/glenlakehomes.csv")
hometypes <- read_csv("sources/hometypes.csv")

totalhomes <- with(glenlakehomes, sum(numberofhomes))
totalsold <- nrow(homesales)
averageturnover <- totalsold / totalhomes
maxyear <- max(homesales$saleyear, na.rm = TRUE)

# market value

homesales %>%
       mutate(year = saleyear) %>%
       group_by(year, hometype) %>%
       summarize(marketvalue = sum(amount, na.rm = TRUE), .groups = "drop") %>%
       ggplot() +
       aes(
              x = year,
              y = marketvalue,
              fill = factor(hometype,
                     levels = c("townhome", "patio home", "residential")
              )
       ) +
       geom_col() +
       scale_y_continuous(labels = dollar_format(
              scale = 1e-3,
              prefix = "$",
              suffix = "K"
       )) +
       labs(
              title = "Glen Lake total market value",
              x = "Year",
              y = "Total market value (in USD)",
              fill = "Home type",
              caption = caption_source
       ) +
       annotate("text",
              x = maxyear,
              y = 1000,
              label = paste(maxyear,
                     "YTD",
                     sep = "\n"
              )
       )

ggsave("graphs/market-value.pdf", width = 11, height = 8)

# median last day of sale in a year

last_sale_day <-
       homesales %>%
       mutate(dayofyear = yday(saledate)) %>%
       group_by(saleyear) %>%
       slice_max(dayofyear, n = 1) %>%
       select(address:amount, dayofyear) %>%
       pull(dayofyear) %>%
       median(.)

marketmodels <- function(tbl) {
       lm(marketvalue ~ dayofyear, data = tbl)
}

valuebyyear <- homesales %>%
       filter(!is.na(saleyear)) %>%
       mutate(dayofyear = yday(saledate)) %>%
       arrange(saledate) %>%
       group_by(saleyear) %>%
       mutate(marketvalue = cumsum(amount)) %>%
       select(saledate, saleyear, dayofyear, marketvalue)

valuebyyear_model <- valuebyyear %>%
       nest() %>%
       mutate(marketmodel = map(data, marketmodels))

augmentdata <- function(tbl) {
       tbl %>% broom::augment(
              newdata = tibble(dayofyear = c(1, last_sale_day)),
              interval = "confidence"
       )
}

modeldata <-
       valuebyyear_model %>%
       mutate(modeldata = map(
              marketmodel,
              augmentdata
       )) %>%
       unnest(modeldata)

modelinfo <-
       valuebyyear_model %>%
       mutate(modelinfo = map(marketmodel, broom::glance)) %>%
       unnest(modelinfo)

modelparameters <-
       valuebyyear_model %>%
       mutate(modelparameters = map(marketmodel, broom::tidy)) %>%
       unnest(modelparameters)

valuebyyear %>%
       ggplot() +
       aes(x = dayofyear, y = marketvalue, color = factor(saleyear)) +
       geom_line() +
       scale_y_continuous(labels = dollar_format(
              scale = 1e-3,
              prefix = "$",
              suffix = "K"
       )) +
       labs(
              title = "Glen Lake total market value",
              x = "Day of year",
              y = "Total market value (in USD)",
              color = "Year",
              caption = caption_source
       ) +
       scale_color_discrete() +
       geom_line(data = modeldata, aes(y = .fitted), lty = 2) +
       theme_light()

ggsave("graphs/market-value-by-dayofyear.pdf", width = 11, height = 8)


this_year <- year(today())
maxyear <- max(homesales$saleyear, na.rm = TRUE)

lwr <- modeldata %>%
       filter(saleyear == this_year, dayofyear == max(dayofyear)) %>%
       pull(.lower) / 1e6

upr <- modeldata %>%
       filter(saleyear == this_year, dayofyear == max(dayofyear)) %>%
       pull(.upper) / 1e6

subtitle <- paste0(
       "Expected value for ",
       this_year,
       " is between $",
       round(lwr, 1),
       "M and $",
       round(upr, 1),
       "M."
)

valuebyyear %>%
       filter(marketvalue == max(marketvalue)) %>%
       mutate(predicted = ifelse(saleyear != this_year, TRUE, FALSE)) %>%
       ggplot() +
       aes(x = saleyear, y = marketvalue, fill = predicted) +
       geom_col() +
       geom_errorbar(aes(
              y = .fitted,
              ymin = .lower,
              ymax = .upper,
              fill = TRUE
       ),
       width = .2,
       data = modeldata %>%
              filter(dayofyear == max(dayofyear))
       ) +
       scale_y_continuous(labels = scales::dollar_format(
              scale = 1e-3,
              prefix = "$",
              suffix = "K"
       )) +
       labs(
              title = "Glen Lake total market value expectation",
              subtitle = subtitle,
              x = "Year",
              y = "Total market value (in USD)",
              fill = "Predicted value",
              caption = caption_source
       ) +
       annotate("text",
              x = this_year,
              y = 1e6,
              label = paste(this_year,
                     "Prediction",
                     sep = "\n"
              )
       ) +
       theme(legend.position = "none")

ggsave("predictions/market-value-prediction.pdf", width = 11, height = 8)


### number of homesales by year

salecountmodel <- function(tbl) {
       lm(salecount ~ dayofyear, data = tbl)
}

salesaugment <- function(tbl) {
       tbl %>%
              broom::augment(
                     newdata = tibble(dayofyear = c(
                            1,
                            365 %/% 4,
                            365 %/% 2,
                            365
                     )),
                     interval = "confidence"
              )
}

saledate <- tibble(
       address = homesales$address,
       listingdate = homesales$saledate,
       type = "sale",
       y = -1
)

salecounter <- saledate %>%
       filter(!is.na(listingdate)) %>%
       arrange(listingdate, na.rm = TRUE) %>%
       mutate(year = year(listingdate)) %>%
       group_by(year) %>%
       mutate(
              salecount = cumsum(-y),
              dayofyear = yday(listingdate),
       ) %>%
       ungroup()

salecountmodels <- salecounter %>%
       group_by(year) %>%
       nest() %>%
       mutate(salesmodel = map(data, salecountmodel))

salesmodeldata <-
       salecountmodels %>%
       mutate(salesmodeldata = map(salesmodel, salesaugment)) %>%
       unnest(salesmodeldata)


salecounter %>%
       ggplot() +
       aes(x = dayofyear, y = salecount, color = factor(year)) +
       geom_line() +
       geom_line(data = salesmodeldata, aes(y = .fitted), lty = 2) +
       scale_y_continuous(limit = c(0, NA)) +
       labs(
              x = "Day of year",
              y = "Numbers of homes sold (cumulative)",
              color = "Year",
              caption = caption_source
       ) +
       theme_light()

ggsave("graphs/sales-by-dayofyear.pdf")


lwr <- salesmodeldata %>%
       filter(year == this_year, dayofyear == max(dayofyear)) %>%
       pull(.lower)

upr <- salesmodeldata %>%
       filter(
              year == this_year,
              dayofyear == max(dayofyear)
       ) %>%
       pull(.upper)

subtitle <- paste0(
       "Expected sales in ",
       this_year,
       " are between ",
       floor(lwr),
       " and ",
       ceiling(upr),
       "."
)

salecounter %>%
       group_by(year) %>%
       filter(salecount == max(salecount)) %>%
       mutate(predicted = ifelse(year != this_year, TRUE, FALSE)) %>%
       ggplot() +
       aes(x = year, y = salecount, fill = predicted) +
       geom_col() +
       geom_errorbar(aes(
              y = .fitted,
              ymin = .lower,
              ymax = .upper,
              fill = TRUE
       ),
       width = .2,
       data = salesmodeldata %>% filter(dayofyear == max(dayofyear))
       ) +
       scale_y_continuous() +
       labs(
              title = "Glen Lake total home sales expectation",
              subtitle = subtitle,
              x = "Year",
              y = "Total home sales",
              fill = "Predicted value",
              caption = caption_source
       ) +
       annotate("text",
              x = this_year,
              y = 5,
              label = paste(this_year,
                     "Prediction",
                     sep = "\n"
              )
       ) +
       theme(legend.position = "none") +
       theme_light()

ggsave("predictions/homesales-prediction.pdf", width = 11, height = 8)