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
    cat("number of taxon usage names:", nrow(object@taxonNames), sep=" ", "\n")
    cat("number of taxon concepts:", nrow(object@taxonRelations), sep=" ", "\n")
    cat("trait entries:", nrow(object@taxonTraits), sep=" ", "\n")
	cat("number of trait variables:", ncol(object@taxonTraits) - 1, sep=" ",
			"\n")
	cat("taxon views:", nrow(object@taxonViews), sep=" ", "\n")
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
overview_taxon <- function(object, ConceptID, display, maxsum, secundum=NULL) {
	if(is.character(ConceptID)) {
		if(ConceptID[1] == "all")
			ConceptID <- object@taxonRelations$TaxonConceptID[1:maxsum] else {
			Names <- list()
			for(i in seq_along(ConceptID)) {
				Names[[i]] <- object@taxonNames$TaxonConceptID[grepl(ConceptID[i],
								object@taxonNames$TaxonName, fixed=TRUE)]
			}
			ConceptID <- do.call(c, Names)
		}
	}
	ConceptID <- na.omit(ConceptID)
    ConceptID <- unique(ConceptID) # Just in case of duplicates
    if(!all(ConceptID %in% object@taxonRelations$TaxonConceptID))
        stop("Some requested concepts are not included in 'object'")
	Names <- accepted_name(object)
	Names$Parent <- object@taxonRelations$Parent[match(Names$TaxonConceptID,
					object@taxonRelations$TaxonConceptID)]
	Names$Basionym <- object@taxonRelations$Basionym[match(Names$TaxonConceptID,
					object@taxonRelations$TaxonConceptID)]
    # Create index for synonyms
    Synonym <- list()
    for(i in ConceptID) {
        temp_name <- object@taxonNames[object@taxonNames$TaxonConceptID == i,]
        temp_name <- temp_name[!temp_name$TaxonUsageID %in%
						Names$TaxonUsageID[Names$TaxonConceptID == i],]
        if(length(temp_name) > 0) Synonym[[paste(i)]] <- temp_name
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
        temp_name <- Names$ViewID[Names$TaxonConceptID == i]
        if(is.na(temp_name)) temp_name <- "none" else {
			if(!is.null(secundum))
				if(!secundum %in% colnames(object@taxonViews)) {
					stop("Value of 'secundum' is not included as column in slot 'taxonViews'")
				} else temp_name <- paste(temp_name, "-",
							object@taxonViews[object@taxonViews$ViewID ==
											temp_name, secundum])
		}
        cat("view ID:", temp_name, sep=" ", "\n")
        temp_name <- paste(Names$Level[Names$TaxonConceptID == i])
        if(is.na(temp_name) | temp_name == "NA") temp_name <- "none"
        cat("level:", temp_name, sep=" ", "\n")
        temp_name <- Names$Parent[Names$TaxonConceptID == i]
		if(is.na(temp_name)) temp_name <- "none" else  temp_name <-
					c(temp_name, paste(Names[Names$TaxonConceptID == temp_name,
									display[-1]], collapse=" "))
		cat("parent:", temp_name, sep=" ", "\n")
        cat("\n")
        # Accepted name
		temp_name <- Names[Names$TaxonConceptID == i, display]
        cat("# accepted name:", "\n")
        cat(paste(temp_name, collapse=" "), "\n")
        # Basionym
		temp_name <- Names$Basionym[Names$TaxonConceptID == i]
        if(!is.na(temp_name)) {
			temp_name <- Names[Names$TaxonConceptID == temp_name, display]
			cat("\n")
            cat("# basionym:", "\n")
            cat(paste(temp_name, collapse=" "), "\n")
        }
        # Synonyms
        if(nrow(Synonym[[paste(i)]]) > 0) {
            cat("\n")
            cat("# synonyms (", nrow(Synonym[[paste(i)]]), "): ", sep="",
                    "\n")
            for(j in seq_len(nrow(Synonym[[paste(i)]]))) {
				temp_name <- Synonym[[paste(i)]][j, display]
                cat(paste(temp_name, collapse=" "), "\n")
            }
        }
    }
    cat("------------------------------", "\n")
}

# Now set the method
setMethod("summary", signature(object="taxlist"),
        function(object, ConceptID, units="Kb", check_validity=TRUE,
                display="both", maxsum=5, secundum=NULL, ...) {
            if(missing(ConceptID))
                overview_taxlist(object, units, check_validity) else
                overview_taxon(object, ConceptID, display, maxsum, secundum)
        }
)
