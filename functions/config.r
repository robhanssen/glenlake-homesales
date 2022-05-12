#
# definition of constants, etc.
#

this_year <- year(today())

homesale_file <- "sources/homesalesdata-source.csv"
caption_source <- paste0(
    "\U00A9 ", # copyright symbol
    this_year,
    ", Rob Hanssen\n",
    "Sources: realtor.com, zillow.com, spartanburgdeeds.com, other public sources." # nolint
)

n_townhomes <- 32
n_patiohomes <- 34
n_residential <- 482 - (n_townhomes + n_patiohomes)