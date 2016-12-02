# TODO:   Print methods specific for taxlist objects
# 
# Author: Miguel Alvarez
################################################################################

# Module for a general summary
overview_taxlist <- function(x) {
    if(nrow(x@taxonRelations) == 1) TAX <- "taxon" else TAX <- "taxa"
    cat(nrow(x@taxonNames), "names for", nrow(x@taxonRelations), TAX, "\n")
    cat(ncol(x@taxonTraits) - 1, "variables for taxon traits", sep=" ", "\n")
    cat(nrow(x@taxonViews), "taxon view(s)", sep=" ", "\n")
    cat("validation for class 'taxlist':", validObject(x), "\n")
    cat("\n")
}

# Module for single taxa
overview_taxon <- function(x, taxon, display, validate) {
    # pre-check
    if(validate) validObject(x)
    # option for all taxa in taxlist
    if(taxon[1] == "all") taxon <- x@taxonRelations$TaxonConceptID
    # transform x to character if integer
    if(length(taxon) == 1 & !all(taxon %in% x@taxonRelations$TaxonConceptID))
        stop("'x' is not included in the input 'taxlist'")
    if(length(taxon) > 1 & !all(taxon %in% x@taxonRelations$TaxonConceptID))
        stop("some concepts are not included in input 'taxlist'")
    # display option
    display <- pmatch(display, c("name","author","both"))[1]
    if(!display %in% c(1:3))
        stop("non-valid value for 'display'")
    # valid names as vector
    AcceptedName_num <- x@taxonRelations[match(taxon,
                    x@taxonRelations$TaxonConceptID),"AcceptedName"]
    AcceptedName <- x@taxonNames[match(
                    AcceptedName_num,x@taxonNames$TaxonUsageID),
            c("TaxonUsageID","TaxonName","AuthorName")]
    if(display != 3) {
        AcceptedName <- paste(AcceptedName[,1], AcceptedName[,display + 1])
    } else {
        AcceptedName <- paste(AcceptedName[,1], AcceptedName[,2],
                AcceptedName[,3])
    }
    names(AcceptedName_num) <- names(AcceptedName) <- paste(taxon)
    # list with synonyms
    Synonyms <- list()
    for(i in names(AcceptedName)) {
        Synonyms[[i]] <- subset(x@taxonNames,
                TaxonConceptID == as.integer(i))[,
                c("TaxonUsageID","TaxonName","AuthorName")]
        Synonyms[[i]] <- subset(Synonyms[[i]],
                TaxonUsageID != AcceptedName_num[i])
        if(display != 3) {
            Synonyms[[i]] <- paste(Synonyms[[i]][,1],
                    Synonyms[[i]][,display+1])
        } else {
            Synonyms[[i]] <- paste(Synonyms[[i]][,1], Synonyms[[i]][,2],
                    Synonyms[[i]][,3])
        }
    }
    # Now print
    for(i in paste(taxon)) {
        cat("------------------------------", "\n")
        cat("# Accepted name for taxon concept '", i, "' (concept view ",
                paste(taxon_relations(x)[x@taxonRelations$TaxonConceptID ==
                                        as.integer(i),"View"]), "):", sep="",
                "\n")
        cat(AcceptedName[i], "\n")
        cat("\n")
        cat("# Synonyms:", "\n")
        if(length(Synonyms[[i]]) == 0) cat("none", "\n") else{
            for(j in Synonyms[[i]]) cat(j, "\n")
        }
    }
    cat("------------------------------", "\n")
}

# Now set the method
setMethod(f="summary", signature(object="taxlist"),
        function(object, taxon, display="both", validate=TRUE) {
            if(missing(taxon)) overview_taxlist(object) else {
                overview_taxon(object, taxon, display, validate)
            }
        })
