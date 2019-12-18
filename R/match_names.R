# TODO:   Match character vectors with names in a taxlist object
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("match_names",
		function(x, object, ...)
			standardGeneric("match_names")
)

# Method for character values
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

# Compare character vector with taxon names of taxlist
setMethod("match_names", signature(x="character", object="taxlist"),
		function(x, object, clean=TRUE, output="data.frame", best=5,
				show_concepts=FALSE, accepted_only=FALSE, method="lcs", ...) {
			if(any(is.na(x)))
				stop("NAs are not allowed in argument 'x'")
			if(clean)
				x <- clean_strings(x)
			SIM <- lapply(split(x, seq_along(x)), function(a, b, method) {
						similarity <- stringsim(a, b@taxonNames$TaxonName, method)
						return(list(TaxonUsageID=b@taxonNames$TaxonUsageID[
												order(similarity, decreasing=TRUE)],
										similarity=similarity[order(similarity,
														decreasing=TRUE)]))
					},
					b=object, method=method, ...)
			output <- pmatch(output[1], c("data.frame","list"))
			if(!output %in% c(1,2))
				stop("non-valid value for 'output'")
			if(output == 2) {
				new_names <- lapply(SIM, function(a, b, best) {
							return(list(TaxonName=with(b@taxonNames,
													TaxonName[match(a$TaxonUsageID[1:best], TaxonUsageID)]),
											TaxonUsageID=a$TaxonUsageID[1:best],
											similarity=a$similarity[1:best]))
						}, b=object, best=best)
				names(new_names) <- x
			}
			if(output == 1) {
				new_names <- lapply(SIM, function(a, b) {
							similarity <- a$similarity[a$similarity == max(a$similarity)]
							TaxonUsageID <- a$TaxonUsageID[a$similarity == max(a$similarity)]
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
				        TaxonName=with(object@taxonNames,
				                TaxonName[match(new_names$TaxonUsageID,
				                                TaxonUsageID)]),
				        AuthorName=with(object@taxonNames,
				                AuthorName[match(new_names$TaxonUsageID,
				                                TaxonUsageID)]),
				        new_names, stringsAsFactors=FALSE)
				if(show_concepts) {
				    new_names$TaxonConceptID <- with(object@taxonNames,
				            TaxonConceptID[match(new_names$TaxonUsageID,
				                            TaxonUsageID)])
				}
			}
			return(new_names)
		}
)
