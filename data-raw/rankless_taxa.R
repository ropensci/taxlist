# TODO:   Issues with rankless taxa having parents
# 
# Author: Miguel Alvarez
################################################################################

library(vegtable)

load_last("../../sam_wetlands/syntax_chile_db/data/cl_syntax")

summary(cl_syntax@species)

object <- cl_syntax@species
