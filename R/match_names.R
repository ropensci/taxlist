# TODO:   Match character vectors with names in a taxlist object
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("match_names",
		function(x, object, ...)
			standardGeneric("match_names")
)

# Compare character vector with taxon names of taxlist
setMethod("match_names", signature(x="character", object="taxlist"),
		function(x, object, clean=TRUE, output="data.frame", best=5,
				show_concepts=FALSE, accepted_only=FALSE, method="lcs", ...) {
			if(any(is.na(x)))
				stop("NAs are not allowed in argument 'x'")
			if(clean) {
				x <- trimws(x, "both")
				x <- gsub("\\s+", " ", x)
			}
			if(accepted_only) {
				ref_name <- with(object@taxonNames,
						TaxonName[TaxonUsageID %in%
										object@taxonRelations$AcceptedName])
			} else ref_name <- object@taxonNames$TaxonName
			SIM <- lapply(split(x, 1:length(x)), stringsim,
					b=ref_name, method=method, ...)
			output <- pmatch(output[1], c("data.frame","list"))
			if(!output %in% c(1,2))
				stop("non-valid value for 'output'")
			if(output == 1) {
				new_names <- data.frame(submittedname=x, stringsAsFactors=FALSE)
				get_best <- function(x, y, z) {
					x_out <- which.max(x)
					x_out <- y@taxonNames[x_out, z]
					return(list(best=x_out, matches=sum(x == max(x))))
				}
				new_names$TaxonName <- sapply(lapply(SIM, get_best, y=object,
								z="TaxonName"), "[[", "best")
				new_names$TaxonUsageID <- sapply(lapply(SIM, get_best, y=object,
								z="TaxonUsageID"), "[[", "best")
				if(show_concepts) {
					new_names$TaxonConceptID <- sapply(lapply(SIM, get_best,
									y=object, z="TaxonConceptID"), "[[", "best")
					new_names$AcceptedName <- with(accepted_name(object),
							TaxonName[match(new_names$TaxonConceptID,
											TaxonConceptID)])
				}
				new_names$matches <- sapply(lapply(SIM, get_best, y=object,
								z="TaxonName"), "[[", "matches")
				new_names$similarity <- sapply(SIM, function(x) x[which.max(x)])
				new_names$TaxonUsageID[new_names$matches > 1] <- NA
				if(show_concepts) {
					new_names$TaxonConceptID[new_names$matches > 1] <- NA
					new_names$AcceptedName[new_names$matches > 1] <- NA
				}
			}
			if(output == 2) {
				new_names <- lapply(SIM, function(SIM, taxlist, best) {
							SIM <- list(similarity=SIM[order(SIM,
													decreasing=TRUE)][1:best],
									TaxonUsageID=taxlist@taxonNames$TaxonUsageID[order(SIM,
													decreasing=TRUE)][1:best])
							return(SIM)
						}, taxlist=object, best=best)
				names(new_names) <- x
			}
			return(new_names)
		}
)
