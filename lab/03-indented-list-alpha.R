# DONE:   The function indented_list() should order taxonomic names
#         alphabetically
# 
# Author: Miguel Alvarez
################################################################################

library(vegtableDB)

DB <- "vegetation_v3"
conn <- connect_db(DB, user = "miguel")

ea_splist <- db2taxlist(conn, "ea_splist")
cype <- subset(ea_splist, TaxonName == "Cyperaceae", keep_parents = TRUE,
    keep_children = TRUE, slot = "taxonNames")

# Arguments
object = cype
keep_children = TRUE
keep_parents = TRUE
indent = " "
lead_br = ""
print = TRUE
author = TRUE
level = FALSE
synonyms = FALSE
syn_encl = c("= ", "")
secundum = "bibtexkey"
alphabetical = FALSE


library(devtools)
install_github("ropensci/taxlist", "devel")
install_github("kamapu/vegtableDB", "devel")

library(taxlist)
library(vegtableDB)

# Restore and connect
DB <- "vegetation_v3"


summary(sp_list, "blabla")

data(Easplsit)
summary(Easplist, "Digitaria")


