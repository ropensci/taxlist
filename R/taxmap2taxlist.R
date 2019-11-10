# TODO:   Parse taxmap objects to taxlist
# 
# Author: Miguel Alvarez
################################################################################

taxmap2taxlist <- function(taxmap, relations, traits, synonyms, views,
		reindex=FALSE) {
	# Extract concept IDs
	old_ids <- taxon_ids(taxmap)
	if(reindex)
		TaxonConceptID <- as.integer(seq_along(old_ids)) else
		TaxonConceptID <- as.integer(old_ids)
	if(any(is.na(TaxonConceptID)))
		stop("Some values of 'taxon_id' cannot be coerced to integers. Try 'reindex=TRUE'")
	
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
	if(!"AuthorName" %in% colnames(taxonRelations) &
			!is.null(taxmap$authorities))
		taxonRelations$AuthorName <- taxmap$authorities
	if(!"AuthorName" %in% colnames(taxonRelations) &
			is.null(taxmap$authorities))
		taxonRelations$AuthorName <- as.character(NA)
	taxonNames <- taxonRelations[,c("TaxonUsageID", "TaxonConceptID",
					"TaxonName", "AuthorName")]
	taxonNames$AuthorName[taxonNames$AuthorName == "NA"] <- NA
	
	# Completing taxonRelations
	taxonRelations$AcceptedName <- taxonRelations$TaxonUsageID
	taxonRelations <- taxonRelations[,!colnames(taxonRelations) %in%
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
						Synonyms[,colnames(taxonNames)]))
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
		Level[,i] <- replace_x(Level[,i], old_ids, taxonRelations$Level)
	Level <- unique(Level)
	Level <- Level[!is.na(Level[,2]),]
	Ranks <- list()
	i <- 1
	repeat{
		Ranks[[i]] <- Level[!Level[,2] %in% Level[,1],2]
		Level <- Level[Level[,2] != Ranks[[i]],]
		i <- i + 1
		if(nrow(Level) == 0) break
	}
	Level <- unique(unlist(Ranks))
	taxonRelations$Level <- factor(taxonRelations$Level, levels=Level)
	
	# assembling taxlist
	obj <- new("taxlist", taxonRelations=taxonRelations, taxonNames=taxonNames)
	if(!missing(views)) obj@taxonViews <- taxmap$data[[views]]
	if(!missing(traits)) obj@taxonTraits <- taxonTraits
	return(obj)
}
