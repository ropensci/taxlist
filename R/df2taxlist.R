# TODO:   Convert a data fram3e into a taxlist object
# 
# Author: Miguel Alvarez
################################################################################

df2taxlist <- function(x, AcceptedName, FirstName, Traits) {
    # Some tests previous to run the function
	if(!is.data.frame(x))
		stop("'x' must be of class 'data.frame'", call.=FALSE)
    if(missing(AcceptedName))
        stop("'AcceptedName' is a mandatory argument", call.=FALSE)
    AcceptedName <- substitute(AcceptedName)
    AcceptedName <- eval(AcceptedName, x, parent.frame())
    if(length(AcceptedName) != nrow(x))
        stop("Argument 'AcceptedName' not matching the size of 'x'")
    Heads <- c("TaxonUsageID","TaxonConceptID","TaxonName","AuthorName")
	if(!all(Heads %in% colnames(x)))
		stop("'TaxonUsageID', 'TaxonConceptID', 'TaxonName', and 'AuthorName' are mandatory columns in 'x'",
				call.=FALSE)
    if(any(duplicated(x$TaxonUsageID)))
        stop("Duplicated usage IDs are not allowed", call.=FALSE)
    # set classes
    if(!is.integer(x$TaxonUsageID)) x$TaxonUsageID <- as.integer(x$TaxonUsageID)
    if(!is.integer(x$TaxonConceptID))
        x$TaxonConceptID <- as.integer(x$TaxonConceptID)
    rownames(x) <- paste(x$TaxonUsageID)
    if(!is.character(x$TaxonName)) x$TaxonName <- paste(x$TaxonName)
    if(!is.character(x$AuthorName)) x$AuthorName <- paste(x$AuthorName)
    # taxon relations
    taxonRelations <- x[AcceptedName,c("TaxonConceptID","TaxonUsageID")]
    colnames(taxonRelations)[2] <- "AcceptedName"
    rownames(taxonRelations) <- paste(taxonRelations$TaxonConceptID)
    if(!missing(FirstName)) {
        FirstName <- substitute(FirstName)
        FirstName <- eval(FirstName, x, parent.frame())
        if(length(FirstName) != nrow(x))
            stop("Argument 'FirstName' not matching the size of 'x'")
        FirstName <- x[FirstName,c("TaxonConceptID","TaxonUsageID")]
        taxonRelations$FirstName <- FirstName$TaxonUsageID[match(
                        taxonRelations$TaxonConceptID,
                        FirstName$TaxonConceptID)]
    } else taxonRelations$FirstName <- as.integer(rep(NA, nrow(taxonRelations)))
    # taxon traits
    traitsTable <- data.frame(TaxonConceptID=taxonRelations[,"TaxonConceptID"],
            row.names=paste(taxonRelations[,"TaxonConceptID"]))
    x <- new("taxlist", taxonNames=x, taxonRelations=taxonRelations,
            taxonTraits=traitsTable)
    if(!missing(Traits)) taxonTraits(x) <- Traits
    return(x)
}
