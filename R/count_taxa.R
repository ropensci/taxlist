#' @name count_taxa
#' 
#' @title Count taxa within a taxlist object.
#' 
#' @description 
#' Counting number of taxa within [taxlist-class] objects or
#' character vectors containing taxon names.
#' 
#' @param object An object containing a taxonomic list or a formula.
#' @param data An object of class [taxlist-class] in the `formula` method.
#' @param rm.na Logical value, whether NAs have to be removed from the input
#'     vector or not.
#' @param level Character value indicating the taxonomic rank of counted taxa.
#' @param ... further arguments passed among methods.
#' 
#' @details 
#' This function is written by convenience in order to reduce code for counting
#' taxa within [taxlist-class] objects and it is just a wrapper of [length()].
#' 
#' @return An integer with the number of taxa.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @examples 
#' ## factor method
#' count_taxa(iris$Species)
#' 
#' ## taxlist method
#' count_taxa(Easplist)
#' count_taxa(Easplist, level="species")
#' 
#' ## using a formula
#' count_taxa(~ lf_behn_2018, Easplist)
#' 
#' @rdname count_taxa
#' 
#' @exportMethod count_taxa
#' 
setGeneric("count_taxa",
		function(object, data, ...)
			standardGeneric("count_taxa")
)

#' @rdname count_taxa
#' 
#' @aliases count_taxa,character,missing-method
#' 
setMethod("count_taxa", signature(object="character", data="missing"),
		function(object, rm.na=TRUE, ...) {
			if(rm.na) object <- object[!is.na(object)]
			return(length(unique(object)))
		}
)

#' @rdname count_taxa
#' 
#' @aliases count_taxa,factor,missing-method
#' 
setMethod("count_taxa", signature(object="factor", data="missing"),
		function(object, rm.na=TRUE, ...) {
			if(rm.na) object <- object[!is.na(object)]
			return(count_taxa(paste(object)))
		}
)

#' @rdname count_taxa
#' 
#' @aliases count_taxa,taxlist,missing-method
#' 
setMethod("count_taxa", signature(object="taxlist", data="missing"),
		function(object, level, ...) {
			if(missing(level))
				n_taxa <- nrow(object@taxonRelations) else {
				if(!level %in% levels(object))
					stop(paste("Value of argument 'level' is not a level in",
									"'object'."))
				n_taxa <- nrow(object@taxonRelations[
								paste(object@taxonRelations$Level) == level, ])
			}
			return(n_taxa)
		}
)

#' @rdname count_taxa
#' 
#' @param include_na Logical value indicating whether `NA` values in a taxon
#'     trait should be considered for counting taxa or just ignored (only
#'     used in `formula` method).
#' @param suffix Character value used as suffix for the counted rank in the
#'     output data frame (only used in `formula` method).
#' 
#' @aliases count_taxa,formula,taxlist-method
#' 
setMethod("count_taxa", signature(object="formula", data="taxlist"),
		function(object, data, include_na=FALSE, suffix="_count", ...) {
			# Some checks
			nr_response <- attr(terms(object), "response")
			if(nr_response > 1)
				stop("More than one response in formula are not allowed.")
			if(nr_response > 0 & !as.character(object)[2] %in%
					taxlist::levels(data))
				stop(paste("Response in 'object' have to be a taxonomic rank",
								"in 'data'."))
			if(any(!attr(terms(object), "term.labels") %in%
							colnames(data@taxonTraits)))
				stop(paste("Some terms in 'object' are not included as traits",
								"in 'data'."))
			# Counting NAs
			if(include_na) {
				traits_df <- data.frame(TaxonConceptID=
								data@taxonRelations$TaxonConceptID)
				for(i in attr(terms(object), "term.labels")) {
					## traits_df[ ,i] <- with(data@taxonTraits,
					##         get(i)[match(traits_df$TaxonConceptID,
					##                         TaxonConceptID)])
					traits_df[ ,i] <- data@taxonTraits[
							match(traits_df$TaxonConceptID,
									data@taxonTraits$TaxonConceptID),i]
					traits_df[ ,i] <- replace_x(paste(traits_df[ ,i]),
							c("", "NA"), rep("NAs", 2))
				}
				data@taxonTraits <- traits_df
			}
			# Counting all taxa
			if(nr_response == 0) {
				object <- as.formula(paste("TaxonConceptID", "~",
								paste(attr(terms(object), "term.labels"),
										collapse=" + ")))
			}
			data <- aggregate(object, data@taxonTraits, length, ...)
			if(nr_response == 0)
				colnames(data)[colnames(data) == "TaxonConceptID"] <-
						paste0("taxa", suffix) else
				colnames(data)[colnames(data) == as.character(object)[2]] <-
						paste0(as.character(object)[2], suffix)
			return(data)
		}
)
