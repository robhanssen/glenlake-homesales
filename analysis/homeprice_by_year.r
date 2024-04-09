library(tidyverse)

load("Rdata/homesales.Rdata")



ggplot(homesales, aes(sample = amount, color = factor(saleyear)) ) +
    geom_qq(show.legend =  FALSE) +
    # geom_qq_line() + 
    scale_y_log10(
        # labels = trans_format("log10", math_format(10^.x))
        labels = scales::label_number()
    )

ggplot(homesales, aes(y = amount, x = factor(saleyear)) ) +
    geom_boxplot(show.legend =  FALSE) +
    # geom_qq_line() + 
    scale_y_log10(
        # labels = trans_format("log10", math_format(10^.x))
        labels = scales::label_number()
    )
