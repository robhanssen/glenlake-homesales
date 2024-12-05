# import all data first
source("02-import-data.r")

safe_source <- safely(source, quiet = FALSE)

scripts <-
    list.files(path = "scripts", pattern = "\\.r$", full.names = TRUE)

purrr::walk(scripts, safe_source)
