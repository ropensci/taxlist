#' Function producing the overview of whole object.
#' 
#' @keywords internal
#' 
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

#' Function producing the overview of single taxon concepts.
#' 
#' @keywords internal
#' 
overview_taxon <- function(object, ConceptID, display, maxsum, secundum=NULL) {
	if(is.character(ConceptID)) {
		if(ConceptID[1] == "all")
			ConceptID <- object@taxonRelations$TaxonConceptID[1:maxsum] else {
			Names <- list()
			for(i in seq_along(ConceptID)) {
				Names[[i]] <- object@taxonNames$TaxonConceptID[
						grepl(ConceptID[i],
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
        temp_name <- object@taxonNames[object@taxonNames$TaxonConceptID == i, ]
        temp_name <- temp_name[!temp_name$TaxonUsageID %in%
						Names$TaxonUsageID[Names$TaxonConceptID == i], ]
        if(length(temp_name) > 0) Synonym[[paste(i)]] <- temp_name
    }
    # display option
    display <- pmatch(display[1], c("name","author","both"))
    if(!display %in% c(1:3))
        stop(paste("Invalid value for 'display', use \"name\", \"author\"",
						"or \"both\""))
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
					stop(paste("Value of 'secundum' is not included as",
									"column in slot 'taxonViews'"))
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

#' @name summary
#' 
#' @rdname summary
#' 
#' @title Print overviews for taxlist Objects and their content
#' 
#' @description 
#' A method to display either an overview of the content of
#' [taxlist-class] objects or an overview of selected taxa.
#' 
#' @param object,x A [taxlist-class] object.
#' @param ConceptID IDs of concepts to be displayed in the summary.
#' @param units Character value indicating the units shown in the object's
#'     allocated space.
#' @param check_validity Logical value indicating whether the validity of
#'     `object` should be checked or not.
#' @param display Character value indicating the field to be displayed (see
#'     details).
#' @param maxsum Integer indicating the maximum number of displayed taxa.
#' @param secundum A character value indicating the column from slot`taxonViews`
#'     to be displayed in the summary.
#' @param ... Further arguments passed to or from another methods.
#' 
#' @details 
#' A general overview indicating number of names, concepts and taxon views
#' included in [taxlist-class] objects.
#' If argument `ConceptID` is a vector with concept IDs or names to be matched
#' by [grepl()], then a display of all names included in each concept will be
#' produced.
#' Alternative you can use `taxon="all"` in order to get the listing of names
#' for all concepts included in the object (truncated to the input number of
#' `maxsum`).
#' 
#' For summaries applied to concepts, there are three alternative displays of
#' names using the argument `display`.
#' Use `display="name"` to show the value `TaxonName`, `display="author"` to
#' show the value `AuthorName` or `display="both"` to show both values.
#' Such values are taken from slot `taxonNames`.
#' 
#' For big objects it will be recommended to set `units="Mb"` (see also
#' [object.size()] for further alternatives).
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @seealso [taxlist-class]
#' 
#' @examples 
#' ## summary of the object
#' summary(Easplist, units="Mb")
#' 
#' ## the same output
#' summary(Easplist)
#' show(Easplist)
#' print(Easplist)
#' Easplist
#' 
#' ## summary for two taxa
#' summary(Easplist, c(51128,51140))
#' 
#' ## summary for a name
#' summary(Easplist, "Acmella")
#' 
#' ## summary for the first 10 taxa
#' summary(object=Easplist, ConceptID="all", maxsum=10)
#' 
#' @aliases summary,taxlist-method
#' 
#' @exportMethod summary
#' 
setMethod("summary", signature(object="taxlist"),
        function(object, ConceptID, units="Kb", check_validity=TRUE,
                display="both", maxsum=5, secundum=NULL, ...) {
            if(missing(ConceptID))
                overview_taxlist(object, units, check_validity) else
                overview_taxon(object, ConceptID, display, maxsum, secundum)
        }
)

#' @rdname summary
#' 
#' @aliases show,taxlist-method
#' 
#' @exportMethod show
#' 
setMethod("show", signature(object="taxlist"),
		function(object) {
			summary(object)
		}
)

#' @exportMethod print
#' 
if(!isGeneric("print"))
	setGeneric("print",
			function(x, ...)
				standardGeneric("print")
	)

#' @rdname summary
#' 
#' @aliases print,taxlist-method
#' 
setMethod("print", signature(x="taxlist"),
		function(x, ...) {
			summary(x, ...)
		}
)
