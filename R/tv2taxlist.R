# TODO:   Import species lists from Turboveg databases
# 
# Author: Miguel Alvarez
################################################################################

# Internal function TCS_replace2 modified from vegdata source
TCS_replace2 <- function(x) {
	x <- replace(x, x == "SPECIES_NR", "TaxonUsageID")
	x <- replace(x, x == "ABBREVIAT", "TaxonName")
	x <- replace(x, x == "AUTHOR", "AuthorName")
	x <- replace(x, x == "VALID_NR", "TaxonConceptID")
}

# The exported function
tv2taxlist <- function(taxlist, tv_home=tv.home()) {
	tv_home <- file.path(tv_home, "species", taxlist)
	species <- read.dbf(file.path(tv_home, "species.dbf"), as.is=TRUE)
	names(species) <- TCS_replace2(names(species))
	species <- df2taxlist(species, !species$SYNONYM)
    if("ecodbase.dbf" %in% list.files(tv_home, pattern=".dbf")) {
		ecodbase <- read.dbf(file.path(tv_home, "ecodbase.dbf"), as.is=TRUE)
		names(ecodbase) <- replace(names(ecodbase),
                names(ecodbase) == "SPECIES_NR", "TaxonConceptID")
        taxon_traits(species) <- ecodbase
    }
    species@taxonRelations$ViewID <- 1
    taxon_views(species) <- data.frame(ViewID=1, Author=taxlist, Year=NA,
            Title=NA, Published=NA, stringsAsFactors=FALSE)
	return(species)
}
