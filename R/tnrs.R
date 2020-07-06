#' @name tnrs
#' 
#' @title Taxonomic Name Resolution Service
#' 
#' @description 
#' Methods of [taxize::tnrs()] for [taxlist-class] objects.
#' 
#' @param query Either a character vector or a taxlist object with names to
#'     search.
#' @param min_score Minimum value of score for considering accepted names as
#'     suggested by the output.
#' @param source Source database.
#' @param ... Further arguments passed to [taxize::tnrs()].
#' 
#' @details 
#' This function checks for matching of taxon names in [taxlist-class]
#' objects with the Taxonomic Name Resolution Service (TNRS).
#' Misspelled names as well as author names will be replaced in the the new
#' object and new accepted names will be inserted.
#' 
#' A method for character vectors is defined for the original function.
#' 
#' @return A data frame or an object of class [taxlist-class].
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @seealso [taxize::tnrs()]
#' 
#' @rdname tnrs
#' 
#' @exportMethod tnrs
#' 
setGeneric("tnrs",
        function(query, ...)
            standardGeneric("tnrs")
)

#' @rdname tnrs
#' 
#' @aliases tnrs,character-method
#' 
setMethod("tnrs", signature(query="character"),
        function(query, ...) taxize::tnrs(query, ...)
)

#' @rdname tnrs
#' 
#' @aliases tnrs,taxlist-method
#' 
setMethod("tnrs", signature(query="taxlist"),
        function(query, min_score=0.8, source="iPlant_TNRS", ...) {
            spp_out <- taxize::tnrs(query@taxonNames$TaxonName, source=source,
                    ...)
            # score for subset
            spp_out$score <- as.numeric(spp_out$score)
            spp_out$score[is.na(spp_out$acceptedname)] <- 0
			## spp_out <- subset(spp_out, score >= min_score)
			spp_out <- spp_out[spp_out$score >= min_score, ]
            for(i in colnames(spp_out)) spp_out[spp_out[ ,i] == "",i] <- NA
			## spp_out <- subset(spp_out, !is.na(acceptedname))
			spp_out <- spp_out[!is.na(spp_out$acceptedname), ]
			# new and old names
            spp_out$new <- !spp_out$acceptedname %in% query@taxonNames$TaxonName
            spp_out$TaxonConceptID <- query@taxonNames$TaxonConceptID[match(
                            spp_out$submittedname, query@taxonNames$TaxonName)]
            # Add new names as synonyms
			## query <- with(spp_out[spp_out$new, ], add_synonym(query,
			##                 TaxonConceptID, acceptedname, authority))
			query <- add_synonym(query, spp_out[spp_out$new,"TaxonConceptID"],
					spp_out[spp_out$new,"acceptedname"],
					spp_out[spp_out$new,"authority"])
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
