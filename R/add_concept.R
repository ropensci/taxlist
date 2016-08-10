# TODO:   Add a new name to a taxlist object
# 
# Author: Miguel Alvarez
################################################################################

add_concept <- function(taxlist, View, TaxonName, AuthorName, ...) {
	if(class(taxlist) != "taxlist")
		stop("'taxlist' must be an object of class taxlist")
    # New concept IDs
    ConceptID <- max(taxlist@taxonNames$TaxonConceptID)
    ConceptID <- (ConceptID + 1):(ConceptID + length(TaxonName))
    if(missing(View)) View <- NA
    View <- rep_len(View, length(ConceptID))
    # New usage IDs
    UsageID <- max(taxlist@taxonNames$TaxonUsageID)
    UsageID <- (UsageID + 1):(UsageID + length(TaxonName))
    # slot taxonRelations
    taxlist@taxonRelations <- do.call(rbind, list(taxlist@taxonRelations,
                    data.frame(TaxonConceptID=ConceptID, AcceptedName=UsageID,
                            View=View)))
    rownames(taxlist@taxonRelations) <- paste(
            taxlist@taxonRelations$TaxonConceptID)
    # slot taxonNames
    for(i in 1:length(ConceptID)) {
        taxlist <- do.call(add_name, append(list(taxlist=taxlist,
                                ConceptID=ConceptID[i], TaxonName=TaxonName[i],
                                AuthorName=AuthorName[i]), sapply(list(...),
                                "[", i, simplify=FALSE)))
    }
    # slot taxonTraits
    OldTraits <- taxon_traits(taxlist)
    taxlist@taxonTraits <- data.frame(
            TaxonConceptID=taxlist@taxonRelations[,"TaxonConceptID"],
            row.names=paste(taxlist@taxonRelations[,"TaxonConceptID"]))
    taxon_traits(taxlist) <- OldTraits
    # return modified taxlist
	return(taxlist)
}
