#' @name df2taxlist
#' 
#' @title Convert data frames into taxlist objects
#' 
#' @description
#' Taxon lists may be provided in data frame format, which will be converted to
#' a [taxlist-class] object.
#' 
#' @param x A data frame or a character vector with taxon names.
#' @param AcceptedName A logical vector indicating accepted names with value
#'     `TRUE`.
#' @param levels A vector with the names of the taxonomic ranks. This argument
#'     is passed to [taxlist::levels()].
#' @param ... Additional vectors to be added as columns in slot`taxonNames`.
#' 
#' @details 
#' In the method `data.frame`, the input data frame must have following columns:
#' \describe{
#'     \item{TaxonUsageID}{Numeric code for the name.}
#'     \item{TaxonConceptID}{Numeric code for the concept.}
#'     \item{TaxonName}{Full name (usage), excluding author name.}
#'     \item{AuthorName}{Author of the combination (taxon name).}
#' }
#' 
#' If the argument `AcceptedName` is missing, all names will be assumed as
#' accepted names.
#' In the alternative `character` method, author names have to be added as
#' additional vectors.
#' 
#' Be aware that the resulting object misses any information on taxon views,
#' basionyms, parent concepts, hierarchical levels and taxon traits.
#' All those elements can be added *a posteriori* by further functions
#' provided in this package.
#' 
#' @return A [taxlist-class] object.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}.
#' 
#' @examples 
#' ## Read the table with names of Cyperus species
#' Cyperus <- read.csv(file=file.path(path.package("taxlist"), "cyperus",
#'     "names.csv"), stringsAsFactors=FALSE)
#' head(Cyperus)
#' 
#' ## Convert to 'taxlist' object
#' Cyperus <- df2taxlist(Cyperus, AcceptedName =! Cyperus$SYNONYM)
#' summary(Cyperus)
#' 
#' ## Create a 'taxlist' object from character vectors
#' Plants <- df2taxlist(c("Triticum aestivum","Zea mays"), AuthorName="L.")
#' summary(Plants, "all")
#' 
#' @rdname df2taxlist
#' 
#' @exportMethod df2taxlist
#' 
setGeneric("df2taxlist",
        function(x, AcceptedName, ...)
            standardGeneric("df2taxlist")
)

#' @rdname df2taxlist
#' 
#' @aliases df2taxlist,data.frame,logical-method
#' 
setMethod("df2taxlist", signature(x="data.frame", AcceptedName="logical"),
        function(x, AcceptedName, levels, ...) {
            # If author names missing
            if(!"AuthorName" %in% colnames(x))
                x$AuthorName <- NA
            if(any(duplicated(x[ ,c("TaxonName","AuthorName")]))) {
                warning("Some duplicated combinations will be deleted")
                x <- x[!duplicated(x[ ,c("TaxonName","AuthorName")]), ]
            }
            # Some tests previous to run the function
            AcceptedName <- substitute(AcceptedName)
            AcceptedName <- eval(AcceptedName, x, parent.frame())
            # When all accepted names
            if(length(AcceptedName) == 1) {
                if(!AcceptedName)
                    stop(paste("for 'AcceptedName' of length 1 only value",
									"'TRUE' is allowed"))
                AcceptedName <- rep(AcceptedName, nrow(x))
            }
            if(length(AcceptedName) != nrow(x))
                stop("Argument 'AcceptedName' not matching the size of 'x'")
            Heads <- c("TaxonUsageID","TaxonConceptID","TaxonName","AuthorName")
            if(!all(Heads %in% colnames(x)))
                stop(paste("'TaxonUsageID', 'TaxonConceptID', 'TaxonName',",
								"and 'AuthorName' are mandatory columns",
								"in 'x'"))
            if(any(duplicated(x$TaxonUsageID)))
                stop("Duplicated usage IDs are not allowed")
            # set classes
            if(!is.integer(x$TaxonUsageID))
                x$TaxonUsageID <- as.integer(x$TaxonUsageID)
            if(!is.integer(x$TaxonConceptID))
                x$TaxonConceptID <- as.integer(x$TaxonConceptID)
            # taxonRelations
            taxonRelations <- x[AcceptedName,c("TaxonConceptID","TaxonUsageID")]
            colnames(taxonRelations)[2] <- "AcceptedName"
			# In the case that ranks are provided
			if("Level" %in% colnames(x)) {
				taxonRelations$Level <- x$Level
				x <- x[,colnames(x) != "Level"]
			}
			# In the case that parents are provided
			if("Parent" %in% colnames(x)) {
				taxonRelations$Parent <- x$Parent
				x <- x[,colnames(x) != "Parent"]
			}
			# taxonNames
            extra_cols <- list(...)
            suppressWarnings({
                        if(length(extra_cols > 0))
                            for(i in names(extra_cols)) x[ ,i] <-
										extra_cols[[i]]
                    }
            )
            taxlist <- new("taxlist")
			## for(i in colnames(taxlist@taxonNames))
			##     if(!i %in% colnames(x)) x[ ,i] <- NA
            for(i in colnames(taxlist@taxonRelations))
                if(!i %in% colnames(taxonRelations)) taxonRelations[ ,i] <- NA
            taxlist@taxonNames <- x
            taxlist@taxonRelations <- taxonRelations
			if(!missing(levels))
				levels(taxlist) <- levels
            return(taxlist)
        }
)

#' @rdname df2taxlist
#' 
#' @aliases df2taxlist,data.frame,missing-method
#' 
setMethod("df2taxlist", signature(x="data.frame", AcceptedName="missing"),
        function(x, ...) return(df2taxlist(x, TRUE, ...))
)

#' @rdname df2taxlist
#' 
#' @aliases df2taxlist,character,missing-method
#' 
setMethod("df2taxlist", signature(x="character", AcceptedName="missing"),
        function(x, ...) {
            if(any(duplicated(x))) {
                warning("Some duplicated names will be deleted")
                x <- x[!duplicated(x)]
            }
            x <- list(TaxonUsageID=seq_along(x), TaxonConceptID=seq_along(x),
                    TaxonName=x, ...)
            x <- as.data.frame(x, stringsAsFactors=FALSE)
            return(df2taxlist(x, TRUE))
        }
)
