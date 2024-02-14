# TODO:   Script for generating data sets in package 'taxlist'
#
# Author: Miguel Alvarez
################################################################################

library(vegtable)

# veg_layers -------------------------------------------------------------------
veg_layers <- read.csv("data-raw/veg_layers.csv")
veg_layers$layer <- as.factor(veg_layers$layer)

save(veg_layers, file = "data/veg_layers.rda")

# Darwin Core names ------------------------------------------------------------
dwc_table <- read.csv("data-raw/dwc-table.csv")

# Save System data
save(dwc_table, file = "R/sysdata.rda")
