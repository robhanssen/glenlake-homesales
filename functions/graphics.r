#
# aux. graph functions
#
#

make_x_breaks <- function(year_range, max_length = 6) {
    divvy <- 1 + length(year_range) %/% max_length
    year_range[which(year_range %% divvy == 0)]

}
