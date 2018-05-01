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
				method="lcs", ...) {
			if(clean) {
				x <- trimws(x, "both")
				x <- gsub("\\s+", " ", x)
			}
			SIM <- lapply(split(x, 1:length(x)), stringsim,
					b=object@taxonNames$TaxonName, method=method, ...)
			output <- pmatch(output[1], c("data.frame","list"))
			if(!output %in% c(1,2))
				stop("non-valid value for 'output'")
			if(output == 1) {
				new_names <- data.frame(submittedname=x, stringsAsFactors=FALSE)
				get_best <- function(x, y, z) {
					x <- which.max(x)
					x <- y@taxonNames[x, z]
					return(x)
				}
				new_names$TaxonUsageID <- sapply(SIM, get_best, y=object,
						z="TaxonUsageID")
				new_names$TaxonName <- sapply(SIM, get_best, y=object,
						z="TaxonName")
				new_names$similarity <- sapply(SIM, function(x) x[which.max(x)])
				
			}
			if(output == 2) {
				new_names <- lapply(SIM, function(SIM, taxlist, best) {
							SIM <- list(similarity=SIM[order(SIM, decreasing=TRUE)][1:best],
									TaxonUsageID=taxlist@taxonNames$TaxonUsageID[order(SIM, decreasing=TRUE)][1:best])
							return(SIM)
						}, taxlist=object, best=best)
				names(new_names) <- x
			}
			return(new_names)
		}
)
