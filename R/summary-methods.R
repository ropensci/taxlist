# TODO:   Print methods specific for taxlist objects
# 
# Author: Miguel Alvarez
################################################################################

# Module for a general summary
overview_taxlist <- function(x) {
    if(nrow(x@taxonRelations) == 1) TAX <- "taxon" else TAX <- "taxa"
    cat(nrow(x@taxonNames), "names for", nrow(x@taxonRelations), TAX, "\n")
    cat(sum(!is.na(x@taxonRelations$FirstName)), " (",
            round(sum(!is.na(x@taxonRelations$FirstName))/
                            nrow(x@taxonRelations)*100),
            "%) taxa with first name entries", sep="", "\n")
    cat(ncol(x@taxonTraits) - 1, "variables for taxon traits", sep=" ", "\n")
    cat("validation for class 'taxlist':", validObject(x), "\n")
    cat("\n")
}

# Module for single taxa
overview_taxon <- function(x, taxon, display, validate) {
    # pre-check
    if(validate) validObject(x)
    # option for all taxa in taxlist
    if(taxon[1] == "all") taxon <- rownames(x@taxonRelations)
    # transform x to character if integer
    if(class(taxon) != "character") taxon <- paste(taxon)
    # concept identities must occur in taxlist
    if(length(taxon) == 1 & !all(taxon %in% rownames(x@taxonRelations)))
        stop("'x' is not included in the input 'taxlist'", call.=FALSE)
    if(!all(taxon %in% rownames(x@taxonRelations)))
        stop("some concepts are not included in input 'taxlist'",
                call.=FALSE)
    # display option
    display <- pmatch(display, c("name","author","both"))[1]
    if(!display %in% c(1:3))
        stop("non-valid value for 'display'", call.=FALSE)
    # valid names as vector
    ValidName <- paste(x@taxonRelations[taxon,"ValidName"])
    ValidName <- x@taxonNames[ValidName,c("TaxonUsageID",
                    "TaxonName","AuthorName")]
    if(display != 3) {
        ValidName <- paste(ValidName[,1], ValidName[,display+1])
    } else {
        ValidName <- paste(ValidName[,1], ValidName[,2], ValidName[,3])
    }
    names(ValidName) <- taxon
    # vector with first names
    FirstName <- paste(x@taxonRelations[taxon,"FirstName"])
    FirstName <- x@taxonNames[FirstName,c("TaxonUsageID",
                    "TaxonName","AuthorName")]
    if(display != 3) {
        FirstName <- paste(FirstName[,1], FirstName[,display+1])
    } else {
        FirstName <- paste(FirstName[,1], FirstName[,2], FirstName[,3])
    }
    FirstName[grepl("NA", FirstName)] <- "none"
    names(FirstName) <- taxon
    # list with synonyms
    Synonyms <- list()
    for(i in taxon) {
        Synonyms[[i]] <- subset(x@taxonNames,
                TaxonConceptID == as.integer(i) & TaxonUsageID !=
                        x@taxonRelations[i,"ValidName"])[,
                c("TaxonUsageID","TaxonName","AuthorName")]
        if(display != 3) {
            Synonyms[[i]] <- paste(Synonyms[[i]][,1],
                    Synonyms[[i]][,display+1])
        } else {
            Synonyms[[i]] <- paste(Synonyms[[i]][,1], Synonyms[[i]][,2],
                    Synonyms[[i]][,3])
        }
    }
    # Now print
    for(i in taxon) {
        cat("------------------------------", "\n")
        cat("# Valid name for taxon concept '", i, "':", sep="", "\n")
        cat(ValidName[i], "\n")
        cat("\n")
        cat("# First name:", "\n")
        cat(FirstName[i], "\n")
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
