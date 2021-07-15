#' @name taxlist2taxmap
#' @rdname taxlist2taxmap
#' 
#' @title Conversion among taxlist and taxmap objects
#' 
#' @description 
#' Exchange of data between the packages `taxlist` and `taxa`.
#' 
#' This function should be used for the exchange of data between the packages
#' [taxlist-package] and `taxa`.
#' 
#' @param taxlist Input object of class [taxlist-class].
#' @param taxmap Input object of class [taxmap].
#' @param relations,traits,synonyms,views Character values indicating the names
#'     of data frames in the [taxmap] object at `data`, which should be
#'     used for the slots taxonRelations, taxonTraits, taxonNames, and
#'     taxonViews, respectively.
#' @param reindex Logical value indicating whether taxon IDs should be assigned
#' 	   anew or not.
#' @param ... Additional arguments passed among methods.
#' 
#' @return 
#' Depending on the applied function, either a [taxlist-class] or a `Taxmap`
#' object.
#' 
#' @author 
#' Miguel Alvarez (\email{kamapu78@@gmail.com}) and Zachary Foster
#' (\email{zacharyfoster1989@@gmail.com}).
#' 
#' @examples 
#' ## Subset Easplist
#' Cyperus <- subset(Easplist, grepl("Cyperus", TaxonName))
#' 
#' ## Convert to taxmap 
#' Cyperus2 <- taxlist2taxmap(Cyperus)
#' Cyperus2
#' 
#' ## Convert it back to taxlist
#' Cyperus2 <- taxmap2taxlist(taxmap=Cyperus2, traits="traits", views="views",
#'     synonyms="synonyms")
#' Cyperus2
#' 
#' @exportMethod taxlist2taxmap
#' 
setGeneric("taxlist2taxmap",
		function(taxlist, ...)
			standardGeneric("taxlist2taxmap")
)

#' @rdname taxlist2taxmap
#' 
#' @aliases taxlist2taxmap,taxlist-method
#' 
setMethod("taxlist2taxmap", signature(taxlist="taxlist"),
		function(taxlist, ...) {
			# Use the edge list to start making the intput object
			taxlist_df <- accepted_name(taxlist)
			## taxlist_df$Parent <- with(taxlist@taxonRelations,
			##         Parent[match(taxlist_df$TaxonConceptID, TaxonConceptID)])
			taxlist_df$Parent <- taxlist@taxonRelations$Parent[
					match(taxlist_df$TaxonConceptID,
							taxlist@taxonRelations$TaxonConceptID)]
			taxlist_df$Level <- as.character(taxlist_df$Level)
			obj <- taxa::parse_edge_list(taxlist_df, taxon_id="TaxonConceptID",
					supertaxon_id="Parent", taxon_name="TaxonName",
					taxon_rank="Level")
			names(obj$data) <- c("relations")
			## obj$data$relations <- with(obj$data,
			##         relations[ ,colnames(relations) != "TaxonConceptID"])
			obj$data$relations <- obj$data$relations[
					,colnames(obj$data$relations) != "TaxonConceptID"]
			# Set taxon authorities
			## obj$set_taxon_auths(with(taxlist_df,
			##                 AuthorName[match(obj$taxon_ids(), TaxonConceptID)]))
			obj$set_taxon_auths(taxlist_df$AuthorName[
							match(obj$taxon_ids(), taxlist_df$TaxonConceptID)])
			# Add traits table to the taxlistect
			obj$data$traits <- taxlist@taxonTraits
			names(obj$data$traits)[1] <- "taxon_id"
			obj$data$traits$taxon_id <- paste(obj$data$traits$taxon_id)
			# Add views table to the taxlistect
			obj$data$views <- taxlist@taxonViews
			# Add synonyms in a table
			obj$data$synonyms <- synonyms(taxlist)[ ,c("TaxonConceptID",
							"TaxonUsageID", "TaxonName", "AuthorName")]
			names(obj$data$synonyms) <- c("taxon_id", "TaxonUsageID", "synonym",
					"synonym_authority")
			obj$data$synonyms$taxon_id <- paste(obj$data$synonyms$taxon_id)
			
			return(obj)
		}
)

#' @rdname taxlist2taxmap
#' 
#' @aliases taxmap2taxlist
#' 
#' @export taxmap2taxlist
#' 
taxmap2taxlist <- function(taxmap, relations, traits, synonyms, views,
		reindex=FALSE) {
	# Extract concept IDs
	old_ids <- taxon_ids(taxmap)
	if(reindex)
		TaxonConceptID <- as.integer(seq_along(old_ids)) else
		TaxonConceptID <- as.integer(old_ids)
	if(any(is.na(TaxonConceptID)))
		stop(paste("Some values of 'taxon_id' cannot be coerced to integers.",
						"Try 'reindex=TRUE'"))
	# Extract taxon relations
	if(missing(relations)) {
		taxonRelations <- data.frame(
				TaxonConceptID=TaxonConceptID,
				TaxonName=taxa::taxon_names(taxmap),
				stringsAsFactors=FALSE)
	} else {
		taxonRelations <- taxmap$data[[relations]]
		colnames(taxonRelations)[colnames(taxonRelations) ==
						"taxon_id"] <- "TaxonConceptID"
		taxonRelations$TaxonConceptID <- TaxonConceptID[
				match(taxonRelations$TaxonConceptID, old_ids)]
	}
	# Extract names IDs and respective information
	if(!"TaxonUsageID" %in% colnames(taxonRelations))
		taxonRelations$TaxonUsageID <- as.integer(seq_len(nrow(taxonRelations)))
	if(!"TaxonName" %in% colnames(taxonRelations))
		taxonRelations$TaxonName <- taxa::taxon_names(taxmap)
	if(!"AuthorName" %in% colnames(taxonRelations))
		taxonRelations$AuthorName <- vapply(taxmap$taxa, "[[", c(AuthorName=""),
				i="authority")
	taxonNames <- taxonRelations[ ,c("TaxonUsageID", "TaxonConceptID",
					"TaxonName", "AuthorName")]
	taxonNames$AuthorName[taxonNames$AuthorName == "NA"] <- NA
	# Completing taxonRelations
	taxonRelations$AcceptedName <- taxonRelations$TaxonUsageID
	taxonRelations <- taxonRelations[ ,!colnames(taxonRelations) %in%
					c("TaxonUsageID", "TaxonName", "AuthorName")]
	taxonRelations$Level <- taxon_ranks(taxmap)
	taxonRelations$Parent <- taxmap$edge_list$from
	taxonRelations$Parent <- TaxonConceptID[match(taxonRelations$Parent,
					old_ids)]
	if(!"Basionym" %in% colnames(taxonRelations))
		taxonRelations$Basionym <- as.integer(NA)
	if(!"ViewID" %in% colnames(taxonRelations))
		taxonRelations$ViewID <- as.integer(NA)
	# Adding synonyms to taxonNames
	if(!missing(synonyms)) {
		Synonyms <- taxmap$data[[synonyms]]
		colnames(Synonyms) <- taxlist::replace_x(colnames(Synonyms),
				c("taxon_id", "synonym", "synonym_authority"),
				c("TaxonConceptID", "TaxonName", "AuthorName"))
		if(!"TaxonUsageID" %in% colnames(Synonyms))
			Synonyms$TaxonUsageID <- as.integer(max(taxonNames$TaxonUsageID) +
							seq_len(nrow(Synonyms)))
		taxonNames <- do.call(rbind, list(taxonNames,
						Synonyms[ ,colnames(taxonNames)]))
	}
	# Adding taxon traits
	if(!missing(traits)) {
		taxonTraits <- taxmap$data[[traits]]
		colnames(taxonTraits)[colnames(taxonTraits) ==
						"taxon_id"] <- "TaxonConceptID"
		taxonTraits$TaxonConceptID <- TaxonConceptID[
				match(taxonTraits$TaxonConceptID, old_ids)]
	}
	# Extract taxonomic ranks
	Level <- taxmap$edge_list
	for(i in c(1,2))
		Level[ ,i] <- replace_x(Level[ ,i], old_ids, taxonRelations$Level)
	Level <- unique(Level)
	Level <- Level[!is.na(Level[ ,2]), ]
	Level <- Level[!is.na(Level[ ,1]), ]
	Level <- rev(unique(unlist(Level)))
	taxonRelations$Level <- factor(taxonRelations$Level, levels=Level)
	# assembling taxlist
	obj <- new("taxlist", taxonRelations=taxonRelations, taxonNames=taxonNames)
	if(!missing(views)) obj@taxonViews <- taxmap$data[[views]]
	if(!missing(traits)) obj@taxonTraits <- taxonTraits
	return(obj)
}
