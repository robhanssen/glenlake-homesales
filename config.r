#
# definition of constants, etc.
#

YEAR = year(today())

homesale_file = "homesalesdata-source.csv"
source = paste0("\U00A9 ", YEAR,", Rob Hanssen\nSources: realtor.com, zillow.com, spartanburgdeeds.com, other public sources")
n_townhomes = 32
n_patiohomes = 34
n_residential = 482 - (n_townhomes + n_patiohomes)
