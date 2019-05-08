# TODO:   Parse taxlist into taxmap object
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("taxlist2taxmap",
		function(taxlist, ...)
			standardGeneric("taxlist2taxmap")
)

setMethod("taxlist2taxmap", signature(taxlist="taxlist"),
		function(taxlist, ...) {
			# Use the edge list to start making the intput object
			taxlist_df <- accepted_name(taxlist)
			taxlist_df$Parent <- with(taxlist@taxonRelations,
					Parent[match(taxlist_df$TaxonConceptID, TaxonConceptID)])
			taxlist_df$Level <- as.character(taxlist_df$Level)
			obj <- taxa::parse_edge_list(taxlist_df, taxon_id="TaxonConceptID",
					supertaxon_id="Parent", taxon_name="TaxonName",
					taxon_rank="Level")
			names(obj$data) <- c("relations")
			obj$data$relations <- with(obj$data,
					relations[,colnames(relations) != "TaxonConceptID"])
			
			# Set taxon authorities
			obj$set_taxon_auths(with(taxlist_df,
							AuthorName[match(obj$taxon_ids(), TaxonConceptID)]))
			
			# Add traits table to the taxlistect
			obj$data$traits <- taxlist@taxonTraits
			names(obj$data$traits)[1] <- "taxon_id"
			obj$data$traits$taxon_id <- paste(obj$data$traits$taxon_id)
			
			# Add views table to the taxlistect
			obj$data$views <- taxlist@taxonViews
			
			# Add synonyms in a table
			obj$data$synonyms <- synonyms(taxlist)[,c("TaxonConceptID",
							"TaxonUsageID", "TaxonName", "AuthorName")]
			names(obj$data$synonyms) <- c("taxon_id", "TaxonUsageID", "synonym",
					"synonym_authority")
			obj$data$synonyms$taxon_id <- paste(obj$data$synonyms$taxon_id)
			
			return(obj)
		}
)
