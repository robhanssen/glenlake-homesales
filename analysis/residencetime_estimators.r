# ============================================================
# Neighborhood residence-time uncertainty analysis
# Data: homesalesdata-source.csv
# Complete analysis period: 2017-2025
#
# Methods:
#   1. Parametric Poisson Monte Carlo
#   2. Nonparametric bootstrap by home
#   3. Nonparametric bootstrap by year
#   4. Two-way bootstrap by home and year
#   5. Bayesian Poisson simulation with Jeffreys prior
# ============================================================

library(tidyverse)
library(patchwork)

set.seed(20261008)


# ------------------------------------------------------------
# 1. User settings
# ------------------------------------------------------------

file_name <- "sources/homesalesdata-source.csv"

n_homes <- 482L
first_year <- 2017L
last_year <- 2025L

years_observed <- first_year:last_year
n_years <- length(years_observed)

# Number of simulation/bootstrap replicates
B <- 100000L


# ------------------------------------------------------------
# 2. Import and clean the data
# ------------------------------------------------------------

sales_raw <- read_csv(
    file_name,
    comment = "#",
    trim_ws = TRUE,
    show_col_types = FALSE
)

sales <- sales_raw |>
    mutate(
        address = trimws(address),
        listingdate = as.Date(listingdate),
        saledate = as.Date(saledate),
        amount = as.numeric(amount),
        hometype = trimws(hometype)
    ) |>
    filter(
        !is.na(saledate),
        lubridate::year(saledate) >= first_year,
        lubridate::year(saledate) <= last_year
    ) |>
    mutate(
        sale_year = lubridate::year(saledate)
    )


# ------------------------------------------------------------
# 3. Basic estimate
# ------------------------------------------------------------

n_sales <- nrow(sales)
exposure_home_years <- n_homes * n_years

annual_turnover_rate <- n_sales / exposure_home_years
estimated_residence_time <- 1 / annual_turnover_rate

cat("Completed sales:", n_sales, "\n")
cat("Number of homes:", n_homes, "\n")
cat("Observation years:", n_years, "\n")
cat("Total exposure:", exposure_home_years, "home-years\n")
cat(
    "Annual turnover rate:",
    round(100 * annual_turnover_rate, 2), "%\n"
)
cat(
    "Estimated average residence time:",
    round(estimated_residence_time, 2), "years\n"
)


# ------------------------------------------------------------
# 4. Construct a complete home-by-year sales matrix
# ------------------------------------------------------------

observed_addresses <- sort(unique(sales$address))
n_observed_addresses <- length(observed_addresses)

n_zero_sale_homes <- n_homes - n_observed_addresses

if (n_zero_sale_homes < 0) {
    stop("The number of observed addresses exceeds n_homes.")
}

zero_sale_addresses <- if (n_zero_sale_homes > 0) {
    paste0("NO_OBSERVED_SALE_", seq_len(n_zero_sale_homes))
} else {
    character(0)
}

all_addresses <- c(
    observed_addresses,
    zero_sale_addresses
)

# Count sales for every home-year combination.
home_year_data <- sales |>
    count(address, sale_year, name = "sales_count") |>
    complete(
        address = all_addresses,
        sale_year = years_observed,
        fill = list(sales_count = 0L)
    ) |>
    arrange(address, sale_year)

# Convert to a 482 x 9 matrix.
sales_matrix <- home_year_data |>
    pivot_wider(
        names_from = sale_year,
        values_from = sales_count,
        values_fill = 0
    ) |>
    column_to_rownames("address") |>
    as.matrix()

storage.mode(sales_matrix) <- "integer"

stopifnot(nrow(sales_matrix) == n_homes)
stopifnot(ncol(sales_matrix) == n_years)
stopifnot(sum(sales_matrix) == n_sales)

cat(
    "Distinct addresses with a sale:",
    n_observed_addresses, "\n"
)
cat(
    "Homes with no observed sale:",
    n_zero_sale_homes, "\n"
)
cat(
    "Matrix dimensions:",
    nrow(sales_matrix), "x", ncol(sales_matrix), "\n"
)

# ------------------------------------------------------------
# 5. Helper functions
# ------------------------------------------------------------

# Convert a simulated sale count into implied residence time.
count_to_residence_time <- function(sale_count) {
    ifelse(
        sale_count > 0,
        exposure_home_years / sale_count,
        NA_real_
    )
}

# Return a consistent statistical summary.
summarize_simulation <- function(x, method) {
    tibble(
        method = method,
        mean = mean(x, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        standard_deviation = sd(x, na.rm = TRUE),
        lower_95 = quantile(x, 0.025, na.rm = TRUE),
        upper_95 = quantile(x, 0.975, na.rm = TRUE)
    )
}

# ------------------------------------------------------------
# 6. Parametric Poisson Monte Carlo
# ------------------------------------------------------------

lambda_hat <- n_sales / exposure_home_years

simulated_sale_counts_poisson <- rpois(
    n = B,
    lambda = lambda_hat * exposure_home_years
)

residence_poisson <- count_to_residence_time(
    simulated_sale_counts_poisson
)

summary_poisson <- summarize_simulation(
    residence_poisson,
    "Parametric Poisson Monte Carlo"
)

summary_poisson

# ------------------------------------------------------------
# 7. Cluster bootstrap by home
# ------------------------------------------------------------

sales_per_home <- rowSums(sales_matrix)

residence_home_bootstrap <- replicate(
    B,
    {
        sampled_home_indices <- sample.int(
            n = n_homes,
            size = n_homes,
            replace = TRUE
        )

        bootstrap_sale_count <- sum(
            sales_per_home[sampled_home_indices]
        )

        count_to_residence_time(bootstrap_sale_count)
    }
)

summary_home <- summarize_simulation(
    residence_home_bootstrap,
    "Home-cluster bootstrap"
)

summary_home

bootstrap_home_once <- function(x) {
    sampled_indices <- sample.int(
        length(x),
        length(x),
        replace = TRUE
    )

    count_to_residence_time(sum(x[sampled_indices]))
}

residence_home_bootstrap <- replicate(
    B,
    bootstrap_home_once(sales_per_home)
)


# ------------------------------------------------------------
# 8. Year-block bootstrap
# ------------------------------------------------------------

sales_per_year <- colSums(sales_matrix)

print(sales_per_year)

residence_year_bootstrap <- replicate(
    B,
    {
        sampled_year_indices <- sample.int(
            n = n_years,
            size = n_years,
            replace = TRUE
        )

        bootstrap_sale_count <- sum(
            sales_per_year[sampled_year_indices]
        )

        count_to_residence_time(bootstrap_sale_count)
    }
)

summary_year <- summarize_simulation(
    residence_year_bootstrap,
    "Year-block bootstrap"
)

summary_year

# ------------------------------------------------------------
# 9. Moving-block bootstrap for adjacent years
# ------------------------------------------------------------

moving_block_year_bootstrap <- function(
  annual_counts,
  block_length = 2L,
  n_bootstrap = 100000L
) {
    n <- length(annual_counts)

    if (block_length > n) {
        stop("block_length cannot exceed the number of years.")
    }

    possible_starts <- seq_len(n - block_length + 1L)
    n_blocks_needed <- ceiling(n / block_length)

    replicate(
        n_bootstrap,
        {
            selected_starts <- sample(
                possible_starts,
                size = n_blocks_needed,
                replace = TRUE
            )

            selected_indices <- unlist(
                lapply(
                    selected_starts,
                    function(start) {
                        start:(start + block_length - 1L)
                    }
                )
            )

            # Trim to the original number of years.
            selected_indices <- selected_indices[seq_len(n)]

            bootstrap_sale_count <- sum(
                annual_counts[selected_indices]
            )

            count_to_residence_time(bootstrap_sale_count)
        }
    )
}

residence_moving_block_2 <- moving_block_year_bootstrap(
    annual_counts = sales_per_year,
    block_length = 2L,
    n_bootstrap = B
)

residence_moving_block_3 <- moving_block_year_bootstrap(
    annual_counts = sales_per_year,
    block_length = 3L,
    n_bootstrap = B
)

summary_moving_block_2 <- summarize_simulation(
    residence_moving_block_2,
    "Moving-block bootstrap, 2-year blocks"
)

summary_moving_block_3 <- summarize_simulation(
    residence_moving_block_3,
    "Moving-block bootstrap, 3-year blocks"
)


# ------------------------------------------------------------
# 10. Two-way bootstrap by home and year
# ------------------------------------------------------------

two_way_bootstrap_once <- function(x) {
    sampled_home_indices <- sample.int(
        nrow(x),
        size = nrow(x),
        replace = TRUE
    )

    sampled_year_indices <- sample.int(
        ncol(x),
        size = ncol(x),
        replace = TRUE
    )

    bootstrap_sale_count <- sum(
        x[sampled_home_indices, sampled_year_indices, drop = FALSE]
    )

    count_to_residence_time(bootstrap_sale_count)
}

residence_two_way <- replicate(
    B,
    two_way_bootstrap_once(sales_matrix)
)

summary_two_way <- summarize_simulation(
    residence_two_way,
    "Two-way home-and-year bootstrap"
)

summary_two_way

# ------------------------------------------------------------
# 11. Bayesian Poisson simulation
# ------------------------------------------------------------

posterior_shape <- n_sales + 0.5
posterior_rate <- exposure_home_years

posterior_lambda <- rgamma(
    n = B,
    shape = posterior_shape,
    rate = posterior_rate
)

posterior_residence_time <- 1 / posterior_lambda

summary_bayesian <- summarize_simulation(
    posterior_residence_time,
    "Bayesian Poisson, Jeffreys prior"
)

summary_bayesian

# ------------------------------------------------------------
# 12. Combined results
# ------------------------------------------------------------

simulation_summary <- bind_rows(
    summary_poisson,
    summary_home,
    summary_year,
    summary_moving_block_2,
    summary_moving_block_3,
    summary_two_way,
    summary_bayesian
) |>
    mutate(
        across(
            c(
                mean,
                median,
                standard_deviation,
                lower_95,
                upper_95
            ),
            ~ round(.x, 2)
        )
    )

print(simulation_summary)

# ------------------------------------------------------------
# 13. Plot the uncertainty distributions
# ------------------------------------------------------------

plot_data <- bind_rows(
    tibble(
        residence_time = residence_poisson,
        method = "Poisson Monte Carlo"
    ),
    tibble(
        residence_time = residence_home_bootstrap,
        method = "Home-cluster bootstrap"
    ),
    tibble(
        residence_time = residence_year_bootstrap,
        method = "Year-block bootstrap"
    ),
    tibble(
        residence_time = residence_two_way,
        method = "Two-way bootstrap"
    ),
    tibble(
        residence_time = posterior_residence_time,
        method = "Bayesian Poisson"
    )
) |>
    filter(
        is.finite(residence_time),
        residence_time <= quantile(residence_time, 0.995)
    )

uncertainty_g <-
    ggplot(
        plot_data,
        aes(
            x = residence_time,
            fill = method,
            color = method
        )
    ) +
    geom_density(
        alpha = 0.15,
        linewidth = 0.8
    ) +
    geom_vline(
        xintercept = estimated_residence_time,
        linetype = "dashed",
        linewidth = 0.7
    ) +
    labs(
        title = "Uncertainty in Estimated Neighborhood Residence Time",
        subtitle = paste0(
            n_sales,
            " sales, ",
            n_homes,
            " homes, ",
            first_year,
            "-",
            last_year
        ),
        x = "Implied average residence time, years",
        y = "Probability density",
        fill = "Method",
        color = "Method"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        legend.position = "bottom"
    )

# ------------------------------------------------------------
# 14. Confidence/credible interval comparison
# ------------------------------------------------------------

interval_plot_data <- simulation_summary |>
    mutate(
        method = reorder(method, median)
    )

estimate_g <-
    ggplot(
        interval_plot_data,
        aes(
            x = median,
            y = method
        )
    ) +
    geom_errorbarh(
        aes(
            xmin = lower_95,
            xmax = upper_95
        ),
        height = 0.18,
        linewidth = 0.8
    ) +
    geom_point(size = 2.5) +
    geom_vline(
        xintercept = estimated_residence_time,
        linetype = "dashed",
        color = "gray40"
    ) +
    labs(
        title = "Estimated Average Residence Time",
        subtitle = "Points are medians; lines are 95% simulation intervals",
        x = "Residence time, years",
        y = NULL
    ) +
    theme_minimal(base_size = 12)

# ------------------------------------------------------------
# 15. Export results
# ------------------------------------------------------------

# write_csv(
#   simulation_summary,
#   "residence_time_simulation_summary.csv"
# )

ggsave(
    filename = "graphs/residence_time_distributions.png",
    width = 14,
    height = 6,
    dpi = 300,
    plot = uncertainty_g + estimate_g
)


simulation_summary |>
    filter(
        method %in% c(
            "Parametric Poisson Monte Carlo",
            "Year-block bootstrap",
            "Two-way home-and-year bootstrap",
            "Bayesian Poisson, Jeffreys prior"
        )
    )
