# TODO:   Solve some problems with special characters in species list
#
# Author: Miguel Alvarez
################################################################################

install.packages("taxlist")

library(taxlist)
library(readODS)

data(Easplist)

head(Easplist@taxonRelations)
