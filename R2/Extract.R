# TODO:   Extract or replace parts of the object
# 
# Author: Miguel Alvarez
################################################################################

# Access by square brackets
setMethod("[", signature(x="taxlist"),
		function(x, i, j, drop=FALSE) {
			if(missing(i)) i <- TRUE
			if(missing(j)) j <- TRUE
			# Resolving problems with NAs
			if(is.logical(i)) i[is.na(i)] <- FALSE else i <- na.omit(i)
			if(is.logical(j)) i[is.na(j)] <- FALSE else j <- na.omit(j)
			x@taxonRelations <- x@taxonRelations[i,,drop]
			if(nrow(x@taxonTraits) > 0) {
				k <- with(x@taxonTraits, TaxonConceptID %in%
								x@taxonRelations$TaxonConceptID)
				x@taxonTraits <- x@taxonTraits[k,j,drop]
			}
			return(clean(x))
		}
)

# Access by dollar
setMethod("$", signature(x="taxlist"), function(x, name) {
			if(!paste(substitute(name)) %in% colnames(x@taxonRelations))
				x@taxonRelations[[name]] <- with(x@taxonTraits,
						get(name)[match(x@taxonRelations$TaxonConceptID,
										TaxonConceptID)])
			return(x@taxonRelations[[name]])
        }
)
