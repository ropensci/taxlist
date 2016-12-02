# TODO:   Add a new name to a taxlist object
# 
# Author: Miguel Alvarez
################################################################################

add_name <- function(taxlist, ConceptID, TaxonName, AuthorName, ...) {
	if(class(taxlist) != "taxlist")
		stop("'taxlist' must be an object of class taxlist")
    if(length(ConceptID) != 1)
        stop("Length of 'ConceptID' should be 1")
	if(!ConceptID %in% taxlist@taxonRelations$TaxonConceptID)
		stop("'ConceptID' is not included as concept in 'taxlist'")
    if(class(ConceptID) != "integer") ConceptID <- as.integer(ConceptID)
    UsageID <- max(taxlist@taxonNames$TaxonUsageID)
    UsageID <- (UsageID + 1):(UsageID + length(TaxonName))
    newName <- list(TaxonUsageID=UsageID, TaxonConceptID=ConceptID,
			TaxonName=TaxonName, AuthorName=AuthorName, ...)
	for(i in colnames(taxlist@taxonNames)[!colnames(taxlist@taxonNames) %in%
					names(newName)]) {
		newName[[i]] <- ""
	}
	newName <- as.data.frame(newName, stringsAsFactors=FALSE)[,
			colnames(taxlist@taxonNames)]
	newName <- do.call(rbind, list(taxlist@taxonNames, newName))
	taxon_names(taxlist) <- newName
	return(taxlist)
}
