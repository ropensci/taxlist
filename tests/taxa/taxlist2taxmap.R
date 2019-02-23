# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

parse_taxlist <- function(intput) {
	# Use the edge list to start making the intput object
	obj = taxa:::parse_edge_list(intput@taxonRelations, taxon_id = "TaxonConceptID", supertaxon_id = "Parent", taxon_name = "TaxonConceptID", taxon_rank = "Level")
	names(obj$data) <- c("relations")
	
	# Set taxon names and authorities
	concept_data <- intput@taxonNames[! duplicated(intput@taxonNames$TaxonConceptID), ]
	obj$set_taxon_names(concept_data$TaxonName[match(obj$taxon_ids(), concept_data$TaxonConceptID)])
	obj$set_taxon_auths(concept_data$AuthorName[match(obj$taxon_ids(), concept_data$TaxonConceptID)])
	
	# Add traits table to the intputect
	obj$data$traits <- intput@taxonTraits
	names(obj$data$traits)[1] <- "taxon_id"
	
	# Add views table to the intputect
	obj$data$views <- intput@taxonViews
	
	# Add synonyms in a table
	obj$data$synonyms <- intput@taxonNames[duplicated(intput@taxonNames$TaxonConceptID), c("TaxonConceptID", "TaxonName", "AuthorName")]
	names(obj$data$synonyms) <- c("taxon_id", "synonym", "synonym_authority")
	
	return(obj)
}

obj <- parse_taxlist(Easplist)
obj
