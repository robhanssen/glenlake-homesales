message("06-dashboard.r")
library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light())

alpha <- .5

homesales <-
    homesales %>%
    #    read_csv("data/homesales_processeddata.csv") %>%
    mutate(
        listingyear = factor(listingyear),
        saleyear = factor(saleyear)
    )

max_date <- max(c(homesales$listingdate, homesales$saledate), na.rm = TRUE)
max_year <- factor(year(max_date))
max_date <- format(max_date, format = "%b %d, %Y")

num_years <- homesales %>%
    distinct(listingyear) %>%
    nrow(.)

colorscale <-
    scales::seq_gradient_pal("blue", "red", "Lab")(seq(0, 1, length.out = num_years))

homeslisted <-
    homesales %>%
    group_by(listingyear) %>%
    summarize(count = n()) %>%
    ggplot() +
    aes(x = listingyear, y = count, fill = listingyear) +
    geom_col(alpha = alpha) +
    labs(
        x = "Year",
        y = "Homes listed",
        title = "Number of homes listed by year"
    ) +
    scale_fill_manual(values = colorscale) +
    theme(legend.position = "none") +
    annotate("text", x = max_year, y = 2, label = "YTD")

homessold <-
    homesales %>%
    filter(!is.na(saleyear)) %>%
    group_by(saleyear) %>%
    summarize(count = n()) %>%
    ggplot() +
    aes(x = saleyear, y = count, fill = saleyear) +
    geom_col(alpha = alpha) +
    labs(
        x = "Year",
        y = "Homes sold",
        title = "Homes sold"
    ) +
    scale_fill_manual(values = colorscale) +
    theme(legend.position = "none") +
    annotate("text", x = max_year, y = 2, label = "YTD")

timeonmarket <-
    homesales %>%
    #    filter(!is.na(listingyear)) %>%
    group_by(listingyear) %>%
    summarize(timeonmarket = median(timeonmarket)) %>%
    ggplot() +
    aes(x = listingyear, y = timeonmarket, fill = listingyear) +
    geom_col(alpha = alpha) +
    labs(
        x = "Year",
        y = "Days",
        title = "Median time on market by year"
    ) +
    scale_fill_manual(values = colorscale) +
    theme(legend.position = "none")

saleprice <-
    homesales %>%
    filter(!is.na(saleyear)) %>%
    group_by(saleyear) %>%
    summarize(saleprice = median(amount), na.rm = TRUE) %>%
    ggplot() +
    aes(x = saleyear, y = saleprice, fill = saleyear) +
    geom_col(alpha = alpha) +
    scale_y_continuous(
        breaks = 50000 * 0:20,
        labels = scales::dollar_format(scale = 1 / 1000, suffix = "K")
    ) +
    labs(
        x = "Year",
        y = "Median price",
        title = "Median sale price by year",
        caption = paste0("Last updated on ", max_date)
    ) +
    scale_fill_manual(values = colorscale) +
    theme(legend.position = "none")

averageinventorysize <-
    homesales %>%
    group_by(listingyear) %>%
    summarize(inventory = mean(inventory)) %>%
    ggplot() +
    aes(x = listingyear, y = inventory, fill = listingyear) +
    geom_col(alpha = alpha) +
    labs(
        x = "Year",
        y = "Average # for sale",
        title = "Average number of homes for sale"
    ) +
    scale_fill_manual(values = colorscale) +
    theme(legend.position = "none")

averageinventorytime <-
    homesales %>%
    group_by(listingyear) %>%
    summarize(inventorytime = median(inventorytime, na.rm = TRUE)) %>%
    mutate(inventorytime = ifelse(listingyear == "2017", 0, inventorytime)) %>%
    ggplot() +
    aes(x = listingyear, y = inventorytime, fill = listingyear) +
    geom_col(alpha = alpha) +
    labs(
        x = "Year",
        y = "Months",
        title = "Average Inventory Time by year"
    ) +
    # annotate("label", x = "2017", y = .5, label = "NO\nDATA") +
    scale_fill_manual(values = colorscale) +
    annotate("label",
        x = "2017",
        y = 5.85,
        label = "buyer's\nseller's\nmarket",
        fill = NA
    ) +
    geom_hline(yintercept = 6, lty = 3) +
    theme(legend.position = "none")

marketvalue <-
    homesales %>%
    filter(!is.na(saleyear)) %>%
    group_by(saleyear) %>%
    summarize(marketvalue = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    aes(x = saleyear, y = marketvalue, fill = saleyear) +
    geom_col(alpha = alpha) +
    scale_y_continuous(
        breaks = 4e6 * 0:20,
        labels = scales::dollar_format(scale = 1 / 1e6, suffix = "M")
    ) +
    labs(
        x = "Year",
        y = "Market value",
        title = "Glen Lake total market value"
    ) +
    scale_fill_manual(values = colorscale) +
    annotate("text", x = max_year, y = 1e6, label = "YTD") +
    theme(legend.position = "none")

turnover <-
    homesales %>%
    filter(!is.na(saleyear)) %>%
    group_by(saleyear) %>%
    count() %>%
    mutate(turnover = n / 482) %>%
    ggplot() +
    aes(x = saleyear, y = turnover, fill = saleyear) +
    geom_col(alpha = alpha) +
    scale_y_continuous(
        breaks = 0.02 * 0:10,
        labels = scales::percent_format()
    ) +
    labs(
        x = "Year",
        y = "Turnover rate",
        title = "Glen Lake home turnover rate"
    ) +
    scale_fill_manual(values = colorscale) +
    annotate("text", x = max_year, y = .01, label = "YTD") +
    theme(legend.position = "none")

overview <- (marketvalue + averageinventorytime + homessold) / (homeslisted + timeonmarket + averageinventorysize) / (turnover + saleprice)

ggsave("dashboard.png", width = 12, plot = overview)