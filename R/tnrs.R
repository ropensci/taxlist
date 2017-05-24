# TODO:   A method for tnrs (taxize) applied to taxlist objects
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("tnrs",
        function(query, ...)
            standardGeneric("tnrs")
)

# Method for the original function
setMethod("tnrs", signature(query="character"),
        function(query, ...) taxize::tnrs(query, ...)
)

# Method for 'taxlist' objects
setMethod("tnrs", signature(query="taxlist"),
        function(query, min_score=0.8, source="iPlant_TNRS", ...) {
            spp_out <- taxize::tnrs(query@taxonNames$TaxonName, source=source,
                    ...)
            # score for subset
            spp_out$score <- as.numeric(spp_out$score)
            spp_out$score[is.na(spp_out$acceptedname)] <- 0
            spp_out <- subset(spp_out, score >= min_score)
            for(i in colnames(spp_out)) spp_out[spp_out[,i] == "",i] <- NA
            spp_out <- subset(spp_out, !is.na(acceptedname))
            # new and old names
            spp_out$new <- !spp_out$acceptedname %in% query@taxonNames$TaxonName
            spp_out$TaxonConceptID <- query@taxonNames$TaxonConceptID[match(
                            spp_out$submittedname, query@taxonNames$TaxonName)]
            # Add new names as synonyms
            query <- with(spp_out[spp_out$new,], add_synonym(query,
                            TaxonConceptID, acceptedname, authority))
            # correct names
            query@taxonNames$TaxonName[na.omit(match(spp_out$submittedname,
                                    query@taxonNames$TaxonName))] <-
                    spp_out$matchedname
            # switch synonyms to accepted names
            spp_out$TaxonUsageID <- query@taxonNames$TaxonUsageID[match(
                            spp_out$acceptedname,query@taxonNames$TaxonName)]
            accepted_name(query, spp_out$TaxonConceptID[spp_out$new]) <-
                    spp_out$TaxonUsageID[spp_out$new]
            # correct author names
            query@taxonNames$AuthorName[na.omit(match(spp_out$TaxonUsageID,
                                    query@taxonNames$TaxonUsageID))] <-
                    spp_out$authority
            # Complete entries in the species list
            query@taxonNames$Link <- spp_out[match(
                            query@taxonNames$TaxonUsageID,spp_out$TaxonUsageID),
                    "uri"]
            return(query)
        }
)
