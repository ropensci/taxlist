#' @name match_names
#' 
#' @title Search matchings between character and taxlist objects
#' 
#' @description 
#' Names provided in a character vector will be compared with names stored in
#' slot `taxonNames` within an object of class [taxlist-class] by
#' using the function [stringsim()].
#' 
#' @param x A character vector with names to be compared.
#' @param object An object of class [taxlist-class] to be compared
#'     with.
#' @param clean Logical value, whether leading, tailing and double blanks should
#'     be deleted from `x`.
#' @param output Character value indicating the type of output (see details).
#' @param best Integer value indicating how many from the best matches have to
#'     be displayed (only working for `output="list"`).
#' @param show_concepts Logical value, whether respective concepts should be
#'     displayed in output or not.
#' @param accepted_only Logical value, whether only accepted names should be
#'     matched or all.
#' @param method,... Further arguments passed to [stringsim()].
#' 
#' @details 
#' For `output="list"` a list with the best matches (taxon usage name ID and
#' similarity) for each queried name will be retrieved, where the number is set
#' by argument `best`.
#' Option `accepted_only=TRUE` will only work with`output="data.frame"`.
#' This will be applied especially in those cases were the requested names have
#' more than one match in the reference [taxlist-class] object
#' (matching homonyms) and will retrieve the one name, that has the status of
#' accepted name, otherwise no matchings will be retrieved.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @seealso [stringsim()]
#' 
#' @examples 
#' ## Names to be compared
#' species <- c("Cperus papyrus", "Typha australis", "Luke skywalker")
#' 
#' ## Retrieve taxon usage names
#' match_names(species, Easplist)
#' 
#' ## Display accepted names in output
#' match_names(x=species, object=Easplist, show_concepts=TRUE)
#' 
#' @rdname match_names
#' 
#' @exportMethod match_names
#' 
setGeneric("match_names",
		function(x, object, ...)
			standardGeneric("match_names")
)

#' @rdname match_names
#' 
#' @aliases match_names,character,character-method
#' 
setMethod("match_names", signature(x="character", object="character"),
		function(x, object, best=5, clean=TRUE, ...) {
			if(length(x) > 1) {
				warning("Only the first element in 'x' will be compared.")
				x <- x[1]
			}
			if(clean) {
				x <- clean_strings(x)
				object <- clean_strings(object)
			}
			c_sim <- stringsim(x, object, ...)
			object <- object[order(c_sim, decreasing=TRUE)]
			return(object[seq_len(best)])
		}
)

#' @rdname match_names
#' 
#' @aliases match_names,character,taxlist-method
#' 
setMethod("match_names", signature(x="character", object="taxlist"),
		function(x, object, clean=TRUE, output="data.frame", best=5,
				show_concepts=FALSE, accepted_only=FALSE, method="lcs", ...) {
			if(any(is.na(x)))
				stop("NAs are not allowed in argument 'x'")
			if(clean)
				x <- clean_strings(x)
			SIM <- lapply(split(x, seq_along(x)), function(a, b, method) {
						similarity <- stringsim(a, b@taxonNames$TaxonName,
								method)
						return(list(TaxonUsageID=b@taxonNames$TaxonUsageID[
												order(similarity,
														decreasing=TRUE)],
										similarity=similarity[order(similarity,
														decreasing=TRUE)]))
					},
					b=object, method=method, ...)
			output <- pmatch(output[1], c("data.frame","list"))
			if(!output %in% c(1,2))
				stop("non-valid value for 'output'")
			if(output == 2) {
				new_names <- lapply(SIM, function(a, b, best) {
							## return(list(TaxonName=with(b@taxonNames,
							##                         TaxonName[
							##                                 match(a$TaxonUsageID[1:best],
							##                                         TaxonUsageID)]),
							##                 TaxonUsageID=a$TaxonUsageID[1:best],
							##                 similarity=a$similarity[1:best]))
							return(list(TaxonName=b@taxonNames$TaxonName[
													match(a$TaxonUsageID[
																	1:best],
															b@taxonNames$TaxonUsageID)],
											TaxonUsageID=a$TaxonUsageID[1:best],
											similarity=a$similarity[1:best]))
						}, b=object, best=best)
				names(new_names) <- x
			}
			if(output == 1) {
				new_names <- lapply(SIM, function(a, b) {
							similarity <- a$similarity[a$similarity ==
											max(a$similarity)]
							TaxonUsageID <- a$TaxonUsageID[a$similarity ==
											max(a$similarity)]
							matches <- length(similarity)
							if(accepted_only) {
								TaxonUsageID <- TaxonUsageID[TaxonUsageID %in%
												b@taxonRelations$AcceptedName]
								matches <- length(TaxonUsageID)
								similarity <- similarity[1]
							}
							if(matches != 1) {
								TaxonUsageID <- NA
								similarity <- similarity[1]
							}
							return(data.frame(TaxonUsageID=TaxonUsageID,
											matches=matches,
											similarity=similarity,
											stringsAsFactors=FALSE))
						}, b=object)
				new_names <- do.call(rbind, new_names)
				new_names <- data.frame(submittedname=x,
						## TaxonName=with(object@taxonNames,
						##         TaxonName[match(new_names$TaxonUsageID,
						##                         TaxonUsageID)]),
						TaxonName=object@taxonNames$TaxonName[
								match(new_names$TaxonUsageID,
										object@taxonNames$TaxonUsageID)],
						## AuthorName=with(object@taxonNames,
						##         AuthorName[match(new_names$TaxonUsageID,
						##                         TaxonUsageID)]),
						AuthorName=object@taxonNames$AuthorName[
								match(new_names$TaxonUsageID,
										object@taxonNames$TaxonUsageID)],
						new_names, stringsAsFactors=FALSE)
				if(show_concepts) {
					## new_names$TaxonConceptID <- with(object@taxonNames,
					##         TaxonConceptID[match(new_names$TaxonUsageID,
					##                         TaxonUsageID)])
					new_names$TaxonConceptID <- object@taxonNames$TaxonConceptID[
							match(new_names$TaxonUsageID,
									object@taxonNames$TaxonUsageID)]
				}
			}
			return(new_names)
		}
)
