# TODO:   Function adding single variables in slot taxonTraits
# 
# Author: Miguel Alvarez
################################################################################

# Set generic method
setGeneric("add_trait",
		function(taxlist, Trait, ConceptID, ...)
			standardGeneric("add_trait")
)

# Method for a data.frame
setMethod("add_trait", signature(taxlist="taxlist", Trait="data.frame",
				ConceptID="missing"),
		function(taxlist, Trait, ...) {
			# Tests
			if(!"TaxonConceptID" %in% colnames(Trait))
				stop("'TaxonConceptID' is a mandatory column in 'Trait'")
			if(any(duplicated(Trait$TaxonConceptID))) {
				warning("Some concepts were duplicated in 'Trait'")
				Trait <- Trait[!duplicated(Trait$TaxonConceptID),]
			}
			if(!all(Trait$TaxonConceptID %in%
							taxlist@taxonRelations$TaxonConceptID))
				warning("Some concepts are not included in 'taxlist'")
			Trait <- Trait[Trait$TaxonConceptID %in%
							taxlist@taxonRelations$TaxonConceptID,]
			if(nrow(taxlist@taxonTraits) == 0) {
				# Concept ID to the front
				Trait <- Trait[,c("TaxonConceptID",
								colnames(Trait)[colnames(Trait) !=
												"TaxonConceptID"])]
				taxlist@taxonTraits <- Trait
			} else {
				Names1 <- colnames(Trait)[colnames(Trait) != "TaxonConceptID"]
				Names2 <- colnames(taxlist@taxonTraits)[
						colnames(taxlist@taxonTraits) != "TaxonConceptID"]
				for(i in 1:length(Names1)) Names1[i] <- add_suffix(Names1[i],
							Names2)
				colnames(Trait)[colnames(Trait) != "TaxonConceptID"] <- Names1
				taxlist@taxonTraits <- merge(taxlist@taxonTraits, Trait,
						all=TRUE)
			}
			return(taxlist)
		}
)

# Method for single vector
setMethod("add_trait", signature(taxlist="taxlist", Trait="vector",
				ConceptID="numeric"),
		function(taxlist, Trait, ConceptID, ...) {
			new_trait <- data.frame(TaxonConceptID=ConceptID, Trait=Trait,
					stringsAsFactors=FALSE)
			colnames(new_trait)[2] <- deparse(substitute(Trait))
			taxlist <- add_trait(taxlist, new_trait, ...)
			return(taxlist)
		}
)
