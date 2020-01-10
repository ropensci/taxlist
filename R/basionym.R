# TODO:   Set basionym for a taxon concept
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("basionym",
        function(taxlist, ConceptID, ...)
            standardGeneric("basionym")
)

# Provide basionyms as data frame
setMethod("basionym", signature(taxlist="taxlist", ConceptID="numeric"),
        function(taxlist, ConceptID, ...) {
			Basionym <- taxlist@taxonRelations[match(ConceptID,
							taxlist@taxonRelations$TaxonConceptID),
					c("TaxonConceptID","Basionym")]
			Basionym$BasionymName <- with(taxlist@taxonNames,
					TaxonName[match(Basionym$Basionym, TaxonUsageID)])
			Basionym$BasionymAuthor <- with(taxlist@taxonNames,
					AuthorName[match(Basionym$Basionym, TaxonUsageID)])
			return(Basionym)
		}
)

# Method for the whole object
setMethod("basionym", signature(taxlist="taxlist", ConceptID="missing"),
        function(taxlist, ConceptID, ...) {
            ConceptID <- taxlist@taxonRelations$TaxonConceptID
            return(basionym(taxlist, ConceptID, ...))
        }
)

# Replacement methods
setGeneric("basionym<-", function(taxlist, ConceptID, value)
            standardGeneric("basionym<-"))

# Replacement for taxlist
setReplaceMethod("basionym", signature(taxlist="taxlist",
                ConceptID="numeric", value="numeric"),
        function(taxlist, ConceptID, value) {
            # first test
            if(length(ConceptID) != length(value))
                stop("'ConceptID' and 'value' should be of the same length.")
			if(!all(taxlist@taxonNames[match(value,
                                    taxlist@taxonNames$TaxonUsageID),
                            "TaxonConceptID"] == ConceptID))
                stop(paste("Some concepts in 'value' are not included in the",
								"respective taxon concept."))
            # now replace
            taxlist@taxonRelations[match(ConceptID,
                            taxlist@taxonRelations$TaxonConceptID),
                    "Basionym"] <- value
            return(taxlist)
        }
)
