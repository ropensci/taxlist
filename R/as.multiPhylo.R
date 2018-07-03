# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

# Generic method
if(!isGeneric("as.multiPhylo"))
	setGeneric("as.multiPhylo",
			function(x, ...)
				standardGeneric("as.multiPhylo")
	)

# Method for taxlist
setMethod("as.multiPhylo", signature(x="taxlist"),
		function(x, ...) {
			TR <- x@taxonRelations[is.na(x@taxonRelations$Parent),
					c("TaxonConceptID","Level")]
			x <- tax2traits(x)
			TR <- split(TR, 1:nrow(TR))
			for(i in 1:length(TR)) {
				sub_set <- x@taxonTraits[x@taxonTraits[,paste(TR[[i]]$Level)] ==
								TR[[i]]$TaxonConceptID,"TaxonConceptID"]
				TR[[i]] <- x
				TR[[i]]@taxonRelations <-
						TR[[i]]@taxonRelations[TR[[i]]@taxonRelations$TaxonConceptID %in%
										sub_set,]
				TR[[i]] <- as.phylo(TR[[i]], ...)
			}
			class(TR) <- "multiPhylo"
			return(TR)
		}
)
