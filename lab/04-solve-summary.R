# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(devtools)
install_github("ropensci/taxlist", "devel")

library(vegtableDB)

conn <- connect_db(dbname = "vegetation_v3", user = "miguel")

sp_list <- db2taxlist(conn, taxonomy = "ea_splist")

summary(sp_list, "Leucas st")



object = sp_list
ConceptID = "Leucas st"
display = "both"
maxsum = 5
secundum = NULL
exact = FALSE







