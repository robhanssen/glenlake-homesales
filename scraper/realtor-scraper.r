library(tidyverse)
library(rvest)

glnames <- read_csv("sources/glenlakehomes-phases.csv") %>% rename(street = streetname)

url <- "https://www.realtor.com/realestateandhomes-search/29316?view=map&pos=35.030767,-81.992637,35.016393,-81.972038,16"

strip_html <- function(xpath) {
    html_text(
        html_nodes(
            html_doc,
            xpath = xpath
        )
    )
}

html_doc <- read_html(url)

data_raw <-
    tibble(
        address_raw = strip_html(".//div[@data-label='pc-address']"),
        price_raw = strip_html(".//span[@data-label='pc-price']"),
    ) %>%
    separate(address_raw,
        into = c("address", "city", "statezip"),
        sep = ","
    ) %>%
    separate(statezip, into = c("none", "state", "zipcode"), sep = " ") %>%
    mutate(
        price = as.numeric(str_remove_all(price_raw, "\\$|\\,")),
        city = str_trim(city),
        street = str_trim(str_remove_all(address, "(\\d|Ct|St|Rd|Blvd|Dr|Pt)"))
    ) %>%
    select(-none, -price_raw) %>%
    semi_join(glnames)
