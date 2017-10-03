# TODO:   Add taxonomy to slot 'taxonTraits'
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("tax2traits",
		function(object, ...)
			standardGeneric("tax2traits")
)

# Method merging a list of taxon concepts
setMethod("tax2traits", signature(object="taxlist"),
		function(object, ...) {
			# taxonomic table
			TAX <- data.frame(
					TaxonConceptID=object@taxonRelations$TaxonConceptID,
					stringsAsFactors=FALSE)
			# first entry with concepts at level
			for(i in paste(levels(object))) {
				ID <- object@taxonRelations[
						paste(object@taxonRelations$Level) == i,
						"TaxonConceptID"]
				TAX[,i] <- ID[match(object@taxonRelations$TaxonConceptID, ID)]
			}
			# second entry parents
			for(i in paste(levels(object))[-length(levels(object))]) {
				TAX <- split(TAX, is.na(TAX[,i]))
				ID <- TAX[["FALSE"]][,i]
				PAR <- object@taxonRelations[match(ID,
								object@taxonRelations$TaxonConceptID),"Parent"]
				LEV <- paste(object@taxonRelations[match(PAR,
										object@taxonRelations$TaxonConceptID),
								"Level"])
				for(j in unique(LEV)) {
					ID_2 <- ID[LEV == j]
					PAR_2 <- PAR[LEV == j]
					TAX[["FALSE"]][,j] <- PAR_2[match(TAX[["FALSE"]][,i],ID_2)]
				}
				TAX <- do.call(rbind, TAX)
			}
			object <- add_trait(object, TAX)
			return(object)
		}
)
