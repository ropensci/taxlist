# TODO:   Print methods specific for taxlist objects
# 
# Author: Miguel Alvarez
################################################################################

# Module for a general summary
overview_taxlist <- function(x, units, validate) {
    cat("object size:", format(object.size(x), units=units), sep=" ", "\n")
    if(validate)
        cat("validation of 'taxlist' object:", validObject(x), sep=" ", "\n")
    cat("\n")
    cat("number of names:", nrow(x@taxonNames), sep=" ", "\n")
    cat("number of concepts:", nrow(x@taxonRelations), sep=" ", "\n")
    cat("trait entries:", nrow(x@taxonTraits), sep=" ", "\n")
    cat("reference entries:", nrow(x@taxonViews), sep=" ", "\n")
    if(any(!is.na(x@taxonRelations$Parent))) {
        cat("\n")
        cat("concepts with parents:",
                length(x@taxonRelations$Parent[
                                !is.na(x@taxonRelations$Parent)]), sep=" ",
                "\n")
        cat("concepts with children:",
                length(unique(x@taxonRelations$Parent[
                                        !is.na(x@taxonRelations$Parent)])),
                sep=" ", "\n")
    }
    if(any(!is.na(x@taxonRelations$Level))) {
        cat("\n")
        cat("hierarchical levels:", paste(levels(x), collapse=" < "), sep=" ",
                "\n")
        for(i in base::levels(x@taxonRelations$Level)) {
            cat("number of concepts in level ", i, ": ",
                    sum(paste(x@taxonRelations$Level) == i), sep="", "\n")
        }
    }
    cat("\n")
}

# Module for single taxa
overview_taxon <- function(object, taxon, display, maxsum) {
    # option for all taxa in taxlist
    if(taxon[1] == "all") taxon <- object@taxonRelations$TaxonConceptID[
                1:maxsum]
    taxon <- taxon[!is.na(taxon)]
    if(!all(taxon %in% object@taxonRelations$TaxonConceptID))
        stop("Some requested consepts are not included in 'object'")
    # display option
    display <- pmatch(display, c("name","author","both"))[1]
    if(!display %in% c(1:3))
        stop("non-valid value for 'display'")
    # valid names as vector
    AcceptedName_num <- object@taxonRelations[match(taxon,
                    object@taxonRelations$TaxonConceptID),"AcceptedName"]
    AcceptedName <- object@taxonNames[match(
                    AcceptedName_num,object@taxonNames$TaxonUsageID),
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
        Synonyms[[i]] <- subset(object@taxonNames,
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
                paste(taxon_relations(object)[
                                object@taxonRelations$TaxonConceptID ==
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
setMethod("summary", signature(object="taxlist"),
        function(object, taxon, units="Mb", validate=TRUE, display="both",
                maxsum=5, ...) {
            if(!missing(taxon))
                overview_taxon(object, taxon, display, maxsum) else 
                overview_taxlist(object, units, validate)
        }
)
