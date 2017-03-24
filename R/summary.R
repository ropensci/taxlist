# TODO:   Print methods specific for taxlist objects
# 
# Author: Miguel Alvarez
################################################################################

# Module for a general summary
overview_taxlist <- function(object, units, check_validity) {
    cat("object size:", format(object.size(object), units=units), sep=" ", "\n")
    if(check_validity)
        cat("validation of 'taxlist' object:", validObject(object), sep=" ",
                "\n")
    cat("\n")
    cat("number of names:", nrow(object@taxonNames), sep=" ", "\n")
    cat("number of concepts:", nrow(object@taxonRelations), sep=" ", "\n")
    cat("trait entries:", nrow(object@taxonTraits), sep=" ", "\n")
    cat("reference entries:", nrow(object@taxonViews), sep=" ", "\n")
    if(any(!is.na(object@taxonRelations$Parent))) {
        cat("\n")
        cat("concepts with parents:",
                length(object@taxonRelations$Parent[
                                !is.na(object@taxonRelations$Parent)]), sep=" ",
                "\n")
        cat("concepts with children:",
                length(unique(object@taxonRelations$Parent[
                                        !is.na(object@taxonRelations$Parent)])),
                sep=" ", "\n")
    }
    if(any(!is.na(object@taxonRelations$Level))) {
        cat("\n")
        cat("hierarchical levels:", paste(levels(object), collapse=" < "),
                sep=" ", "\n")
        for(i in base::levels(object@taxonRelations$Level)) {
            cat("number of concepts in level ", i, ": ",
                    sum(paste(object@taxonRelations$Level) == i), sep="", "\n")
        }
    }
    cat("\n")
}

# Module for single taxa
overview_taxon <- function(object, ConceptID, display, maxsum) {
    if(ConceptID[1] == "all")
        ConceptID <- object@taxonRelations$TaxonConceptID[1:maxsum]
    ConceptID <- na.omit(ConceptID)
    ConceptID <- unique(ConceptID) # Just in case of duplicates
    if(!all(ConceptID %in% object@taxonRelations$TaxonConceptID))
        stop("Some requested concepts are not included in 'object'")
    object@taxonRelations <- object@taxonRelations[
            object@taxonRelations$TaxonConceptID %in% ConceptID,]
    ## object <- clean(object)
    # Create index for synonyms
    Synonym <- list()
    for(i in ConceptID) {
        temp_names <- object@taxonNames[object@taxonNames$TaxonConceptID == i,
                "TaxonUsageID"]
        temp_names <- temp_names[!temp_names %in%
                        object@taxonRelations[
                                object@taxonRelations$TaxonConceptID == i,
                                "AcceptedName"]]
        if(length(temp_names) > 0) Synonym[[paste(i)]] <- temp_names
    }
    # display option
    display <- pmatch(display[1], c("name","author","both"))
    if(!display %in% c(1:3))
        stop("non-valid value for 'display'")
    if(display == 1) display <- c("TaxonUsageID", "TaxonName")
    if(display == 2) display <- c("TaxonUsageID", "AuthorName")
    if(display == 3) display <- c("TaxonUsageID", "TaxonName", "AuthorName")
    # Now print
    for(i in ConceptID) {
        cat("------------------------------", "\n")
        # Head
        cat("concept ID:", i, sep=" ", "\n")
        temp_name <- object@taxonRelations[
                object@taxonRelations$TaxonConceptID == i,"ViewID"]
        if(is.na(temp_name)) temp_name <- "none"
        cat("view ID:", temp_name, sep=" ", "\n")
        temp_name <- paste(object@taxonRelations[
                        object@taxonRelations$TaxonConceptID == i,"Level"])
        if(is.na(temp_name) | temp_name == "NA") temp_name <- "none"
        cat("level:", temp_name, sep=" ", "\n")
        temp_name <- object@taxonRelations[
                object@taxonRelations$TaxonConceptID == i,"Parent"]
        if(is.na(temp_name)) temp_name <- "none"
        cat("parent:", temp_name, sep=" ", "\n")
        cat("\n")
        # Accepted name
        temp_name <- object@taxonRelations[
                object@taxonRelations$TaxonConceptID == i,"AcceptedName"]
        temp_name <- object@taxonNames[
                object@taxonNames$TaxonUsageID == temp_name, display]
        temp_name[is.na(temp_name)] <- ""
        cat("# accepted name:", "\n")
        cat(paste(temp_name, collapse=" "), "\n")
        # Basionym
        temp_name <- object@taxonRelations[
                object@taxonRelations$TaxonConceptID == i,"Basionym"]
        if(!is.na(temp_name)) {
            temp_name <- object@taxonNames[
                    object@taxonNames$TaxonUsageID == temp_name, display]
            temp_name[is.na(temp_name)] <- ""
            cat("\n")
            cat("# basionym:", "\n")
            cat(paste(temp_name, collapse=" "), "\n")
        }
        # Synonyms
        if(paste(i) %in% names(Synonym)) {
            cat("\n")
            cat("# synonyms (", length(Synonym[[paste(i)]]), "): ", sep="",
                    "\n")
            for(j in 1:length(Synonym[[paste(i)]])) {
                temp_name <- object@taxonNames[
                        object@taxonNames$TaxonUsageID ==
                                Synonym[[paste(i)]][j], display]
                temp_name[is.na(temp_name)] <- ""
                cat(paste(temp_name, collapse=" "), "\n")
            }
        }
    }
    cat("------------------------------", "\n")
}

# Now set the method
setMethod("summary", signature(object="taxlist"),
        function(object, ConceptID, units="Kb", check_validity=TRUE,
                display="both", maxsum=5, ...) {
            if(missing(ConceptID))
                overview_taxlist(object, units, check_validity) else
                overview_taxon(object, ConceptID, display, maxsum)
        }
)
