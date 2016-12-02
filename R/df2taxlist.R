# TODO:   Convert a data fram3e into a taxlist object
# 
# Author: Miguel Alvarez
################################################################################

df2taxlist <- function(x, AcceptedName, Traits) {
    # Some tests previous to run the function
	if(!is.data.frame(x))
		stop("'x' must be of class 'data.frame'")
    if(missing(AcceptedName))
        stop("'AcceptedName' is a mandatory argument")
    AcceptedName <- substitute(AcceptedName)
    AcceptedName <- eval(AcceptedName, x, parent.frame())
    if(length(AcceptedName) != nrow(x))
        stop("Argument 'AcceptedName' not matching the size of 'x'")
    Heads <- c("TaxonUsageID","TaxonConceptID","TaxonName","AuthorName")
	if(!all(Heads %in% colnames(x)))
		stop("'TaxonUsageID', 'TaxonConceptID', 'TaxonName', and 'AuthorName' are mandatory columns in 'x'")
    if(any(duplicated(x$TaxonUsageID)))
        stop("Duplicated usage IDs are not allowed")
    # set classes
    if(!is.integer(x$TaxonUsageID)) x$TaxonUsageID <- as.integer(x$TaxonUsageID)
    if(!is.integer(x$TaxonConceptID))
        x$TaxonConceptID <- as.integer(x$TaxonConceptID)
    if(!is.character(x$TaxonName)) x$TaxonName <- paste(x$TaxonName)
    if(!is.character(x$AuthorName)) x$AuthorName <- paste(x$AuthorName)
    # taxon relations
    taxonRelations <- x[AcceptedName,c("TaxonConceptID","TaxonUsageID")]
    colnames(taxonRelations)[2] <- "AcceptedName"
    taxonRelations$View <- as.integer(rep(NA, nrow(taxonRelations)))
    # taxon traits
    traitsTable <- data.frame(TaxonConceptID=taxonRelations[,"TaxonConceptID"],
            row.names=paste(taxonRelations[,"TaxonConceptID"]))
    x <- new("taxlist", taxonNames=x, taxonRelations=taxonRelations,
            taxonTraits=traitsTable)
    if(!missing(Traits)) taxon_traits(x) <- Traits
    return(x)
}
