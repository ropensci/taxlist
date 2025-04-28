# TODO:   Usage of 'taxlist2df()'
# 
# Author: Miguel Alvarez
################################################################################

library(taxlist)

data(Easplist)

spp <- as(Easplist, "data.frame")

head(taxlist2df(Easplist))
head(taxlist2df(Easplist, standard = "dwc"))
