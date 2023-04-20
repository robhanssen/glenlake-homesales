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

data_listed <-
    tibble(
        address_raw = strip_html(".//div[@data-label='pc-address']"),
        price_raw = strip_html(".//span[@data-label='pc-price']"),
    ) %>%
    separate(address_raw,
        into = c("address", "city", "statezip"),
        sep = ","
    ) %>%
    mutate(across(everything(), str_trim)) %>%
    separate(statezip, into = c("state", "zipcode"), sep = " ") %>%
    mutate(
        price = as.numeric(str_remove_all(price_raw, "\\$|\\,")),
        street = str_trim(str_remove_all(address, "(\\d{2,8}|Ct|St|Rd|Blvd|Dr|Pt|Ln|Pl)"))
    ) %>%
    select(-price_raw) %>%
    semi_join(glnames, by = join_by(street)) %>%
    arrange(desc(price))

price_quants <-
    data_listed %>%
    summarize(
        price_25 = quantile(price, 0.1),
        price_50 = quantile(price, 0.50),
        price_75 = quantile(price, 0.9),
    )


ggplot(data_listed, aes(y = fct_reorder(address, price), x = price)) + 
    geom_col() + 
    geom_vline(xintercept = price_quants$price_25) + 
    geom_vline(xintercept = price_quants$price_50) + 
    geom_vline(xintercept = price_quants$price_75) + 
    scale_x_continuous(labels = scales::label_dollar())