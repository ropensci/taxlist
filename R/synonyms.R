# TODO:   Retrieve synonyms
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("synonyms",
		function(taxlist, ConceptID, ...)
			standardGeneric("synonyms")
)

# Provide accepted names in a data frame
setMethod("synonyms", signature(taxlist="taxlist", ConceptID="numeric"),
		function(taxlist, ConceptID, ...) {
			Syn <- taxlist@taxonNames[taxlist@taxonNames$TaxonConceptID %in%
							ConceptID,c("TaxonUsageID","TaxonConceptID",
							"TaxonName","AuthorName")]
			Syn$AcceptedName <- with(taxlist@taxonRelations,
					AcceptedName[match(Syn$TaxonConceptID, TaxonConceptID)])
			Syn$AuthorAcceptedName <- with(taxlist@taxonNames,
					AuthorName[match(Syn$AcceptedName, TaxonUsageID)])
			Syn$AcceptedName <- with(taxlist@taxonNames,
					TaxonName[match(Syn$AcceptedName, TaxonUsageID)])
			Syn <- Syn[!Syn$TaxonUsageID %in% with(taxlist@taxonRelations,
							AcceptedName[TaxonConceptID %in% ConceptID]),]
			return(Syn)
		}
)

# Method for the whole object
setMethod("synonyms", signature(taxlist="taxlist", ConceptID="missing"),
		function(taxlist, ConceptID, ...) {
			ConceptID <- taxlist@taxonRelations$TaxonConceptID
			return(synonyms(taxlist, ConceptID, ...))
		}
)
