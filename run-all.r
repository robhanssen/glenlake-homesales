# import all data first
source("02-import-data.r")

scripts <-
    list.files(path = "scripts", pattern = "*.r$", full.names = TRUE)

purrr::walk(scripts, source)
