library(tidyverse)
library(patchwork)
library(furrr)
plan(multisession)

load("Rdata/homesales.Rdata")

n_bootstrap <- 10000

fuel_new <- fuel %>%
    filter(!str_detect(car_name, "2008"), year > 2013)

bootstrapping <- function(dat, n = 100) {
    max <- nrow(dat)

    sample_list <- map(1:n, \(q) sample(1:max, max, replace = TRUE))

    map_dbl(sample_list, \(sample) {
        with(dat[sample, ], mean(amount))
    })
}

real_means <- fuel_new %>%
    summarize(
        mean = sum(miles) / sum(gallons),
        .by = car_name
    )


generate_bootstraps <- function(dat, var, custom_var) {
    bootstraps <- dat %>%
        nest(data = !c({{var}}, {{ custom_var }})) %>%
        mutate(
            sale_amount = future_map(data, bootstrapping, n = n_bootstrap, .options = furrr_options(seed = TRUE))
        )

    home_colors <- c(
        "residential" = "#1b9e77",  
        "patiohome" = "#d95f02",
        "townhome" = "#7570b3"
    )

    mean_labels <- bootstraps %>%
        mutate(meanse = map(sale_amount, quantile, probs = c(.025, .5, 0.975), na.rm = TRUE)) %>%
        unnest_wider(meanse) %>%
        mutate(
            label = glue::glue("{round(`2.5%`, digits = 1)} - {round(`97.5%`, digits = 1)}"),
            year = factor({{var}})
        )
 
    bootstraps %>%
        unnest_longer(sale_amount) %>%
        ggplot(
            aes(x = sale_amount, y = factor({{var}}), fill = {{custom_var}}, color = hometype)
        ) +
        ggridges::geom_density_ridges(alpha = .5) +
        # geom_vline(
        #     data = real_means,
        #     aes(xintercept = mean, color = hometype),
        #     linewidth = 2, alpha = .33
        # ) +
        scale_fill_manual(values = home_colors) +
        scale_color_manual(values = home_colors) +
        # scale_x_continuous(
        #     breaks = seq(0, 100, 2)
        # ) +
        # geom_label(
        #     data = real_means,
        #     aes(x = mean, label = car_name, color = car_name), y = 0.7,
        #     fill = "white", label.size = NA
        # ) +
        # geom_text(
        #     data = mean_labels,
        #     aes(
        #         x = .5 + `97.5%`, y = year,
        #         label = label, color = car_name
        #     ),
        #     hjust = 0, vjust = -.5
        # ) +
        coord_cartesian(clip = "off") +
        labs(
            x = "Bootstrapped fuel economy distribution (in miles per gallon)",
            y = NULL,
            # title = glue::glue("Estimated fuel economy distribution by {custom_var}")
        ) +
        theme(
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0, size = 6),
            plot.title.position = "plot"
        )
}

homesales %>%
    filter(!is.na(amount)) %>%
    mutate(
        hometype = case_when(
            str_detect(hometype, "patio home") ~ "residential",
            TRUE ~ hometype
        )
    ) %>% 
    generate_bootstraps(saleyear, hometype) 