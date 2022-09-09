library(tidyverse)
library(tidyverse)
library(ggridges)

load("Rdata/homesales.Rdata")

homesales %>%
    filter(!is.na(saleyear)) %>%
    ggplot + 
    aes(y = factor(saleyear), x = timeonmarket) + 
    geom_density_ridges() + 
    geom_boxplot()


homesales %>%
    filter(!is.na(saleyear)) %>%
    ggplot + 
    aes(x = factor(saleyear), y = timeonmarket) + 
    geom_jitter(width  = .1) + 
    geom_violin(alpha = .2) + 
    geom_violin(data = x, color = "red")



a <- homesales %>% filter(saleyear == 2021) %>% pull(timeonmarket) %>% as.numeric()

coef(glm(a~1,family=poisson(link="identity")))    


homesales %>%
    filter(!is.na(saleyear)) %>%
    select(saleyear, timeonmarket) %>%
    mutate(time = as.numeric(timeonmarket)) %>%
    group_by(saleyear) %>%
    nest() %>%
    mutate(pois = map_dbl(data, ~coef(glm(.x$time~1,family=poisson(link="identity")))))

x <- tibble(saleyear = 2019, timeonmarket  = rpois(100, 138))

ggplot


estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

a <- homesales %>% filter(saleyear == 2021) %>% pull(timeonmarket) %>% as.numeric()

m <- mean(a)
s <- var(a)

estBetaParams(m, s)