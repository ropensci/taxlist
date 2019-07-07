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
		function(object, get_names=FALSE, ...) {
			# taxonomic table
			TAX <- data.frame(
					TaxonConceptID=object@taxonRelations$TaxonConceptID,
					stringsAsFactors=FALSE)
			# first entry with concepts at level
			if(all(is.na(object@taxonRelations$Level)) |
					all(is.na(object@taxonRelations$Parent))) {
				warning("Input object without taxonomic ranks.")
				return(object)
			} else {
				for(i in taxlist::levels(object)) {
					ID <- object@taxonRelations[
							paste(object@taxonRelations$Level) == i,
							"TaxonConceptID"]
					TAX[,i] <- ID[match(object@taxonRelations$TaxonConceptID, ID)]
				}
				# second entry parents
				for(i in levels(object)[-length(levels(object))]) {
					if(!all(is.na(TAX[,i])) & !all(!is.na(TAX[,i]))) {
						TAX <- split(TAX, is.na(TAX[,i]))
						ID <- TAX[["FALSE"]][,i]
						PAR <- object@taxonRelations[match(ID,
										object@taxonRelations$TaxonConceptID),
								"Parent"]
						LEV <- paste(object@taxonRelations[match(PAR,
												object@taxonRelations$TaxonConceptID),
										"Level"])
						LEV[LEV == "NA"] <- NA
						for(j in unique(LEV[!is.na(LEV)])) {
							ID_2 <- ID[LEV == j]
							PAR_2 <- PAR[LEV == j]
							TAX[["FALSE"]][,j] <- PAR_2[match(TAX[["FALSE"]][,i],
											ID_2)]
						}
						TAX <- do.call(rbind, TAX)
					} else {
						if(all(!is.na(TAX[,i]))) {
							ID <- TAX[,i]
							PAR <- object@taxonRelations[match(ID,
											object@taxonRelations$TaxonConceptID),
									"Parent"]
							LEV <- paste(object@taxonRelations[match(PAR,
													object@taxonRelations$TaxonConceptID),
											"Level"])
							LEV[LEV == "NA"] <- NA
							for(j in unique(LEV[!is.na(LEV)])) {
								ID_2 <- ID[LEV == j]
								PAR_2 <- PAR[LEV == j]
								TAX[,j] <- PAR_2[match(TAX[,i], ID_2)]
							}
						}
					}
				}
				colnames(TAX)[colnames(TAX) == "TaxonConceptID"] <- "ConceptID"
				object <- do.call(update_trait, c(list(taxlist=object),
								as.list(TAX)))
				if(get_names) {
					Names <- accepted_name(object)
					for(i in paste(levels(object))) {
						object@taxonTraits[,i] <- Names[
								match(object@taxonTraits[,i], Names$TaxonConceptID),
								"TaxonName"]
					}
				}
				return(object)
			}
		}
)
