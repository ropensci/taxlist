# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(taxlist)

Cyperus <- read.csv(system.file("cyperus", "names.csv", package = "taxlist"),
    stringsAsFactors = FALSE
)
Cyperus$AcceptedName <- !Cyperus$SYNONYM

new_tax <- df2taxlist(Cyperus)
