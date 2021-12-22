# TODO: Add comment
#
# Author: Miguel Alvarez
################################################################################

# Example taken from facebook discussion
library(taxlist)

# Data frame
Data <- data.frame(
  GenusSpecies = c(
    "Acer platanoides - bla bla",
    " Acer  platanoides Miguel ", "Acer platanifolius ble"
  ),
  n = 1
)

# Get rid of leading, trailing and double blanks
Data$GenusSpecies <- clean_strings(Data$GenusSpecies)

# This part can be done more efficient
Data$new_name <- dissect_name(Data$GenusSpecies, repaste = c(1:2))

# Statistics
Stats <- aggregate(n ~ new_name, Data, sum)
