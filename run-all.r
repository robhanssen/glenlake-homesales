future::plan(future::multisession)

# import all data first
source("02-import-data.r")

safe_source <- purrr::safely(source, quiet = FALSE)

scripts <-
    list.files(path = "scripts", pattern = "\\.r$", full.names = TRUE)

furrr::future_walk(scripts, safe_source, .options = furrr::furrr_options(seed = TRUE))
