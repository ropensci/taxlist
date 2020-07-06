#' @name accepted_name
#' 
#' @title Manage accepted names, synonyms and basionyms
#' 
#' @description 
#' Taxon usage names for a taxon concept can be divided into three categories:
#' accepted names, basionyms and synonyms.
#' Each single taxon concept may at least have an accepted name, while basionym
#' and synonyms are optional.
#' The functions `accepted_name`, `basionym` and `synonyms`  can be used either
#' to display the respective usage names or to set usage names in one of those
#' categories.
#' 
#' @param taxlist An object of class [taxlist-class].
#' @param ConceptID Integer containing concept IDs where to request or set names
#'     for one category.
#' @param show_traits Logical value, whether traits should be included in the
#'     output of `accepted_name` or not.
#' @param value Integer containing usage IDs to be set to the respective
#'     category in the respective taxon concept.
#' @param ... Further arguments passed among methods.
#' 
#' @details 
#' The function `accepted_name` retrieves the accepted names for the indicated
#' taxon concepts or for the whole [taxlist-class] object.
#' By using `show_traits=TRUE`, the respective taxon traits will be
#' displayed as well, providing an overview of taxa included in the object.
#' The replacement method for this function will set the respective usage name
#' IDs as accepted names for the respective taxon concept, provided that these
#' names are already set as synonyms in the respective concepts.
#' 
#' The function `synonyms` is working in a similar way as `accepted_name`, but
#' this function does not include taxon traits in the output and there is no
#' replacing method for `synonyms`.
#' Alternatives for inserting new synonyms into a taxon concept are either
#' moving synonyms from other taxa by using [change_concept<-] or
#' inserting new names in the object by using [add_synonym()].
#' 
#' The function `basionym` is retrieving and setting basionyms in the
#' respective taxon concepts similarly to `accepted_name`, but this function
#' does not retrieve any information on taxon traits, either.
#' 
#' @return
#' Most of the methods return information in data frames, while
#' replacement methods do it as [taxlist-class] objects.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @seealso [add_synonym()] [change_concept<-]
#' 
#' @examples 
#' ## Set a different accepted name for Cyclosorus interruptus
#' summary(Easplist, "Cyclosorus interruptus")
#' accepted_name(Easplist, 50074) <- 53097
#' summary(Easplist, 50074)
#' 
#' ## Inserting a new name first
#' summary(Easplist, "Basella alba")
#' Easplist <- add_synonym(taxlist=Easplist, ConceptID=68,
#'     TaxonName="Basella cordifolia", AuthorName="Lam.")
#' summary(Easplist, 68)
#' accepted_name(Easplist, 68) <- 56139
#' summary(Easplist, 68)
#' 
#' @rdname accepted_name
#' 
#' @exportMethod accepted_name
#' 
setGeneric("accepted_name",
        function(taxlist, ConceptID, ...)
            standardGeneric("accepted_name")
)

#' @rdname accepted_name
#' 
#' @aliases accepted_name,taxlist,numeric-method
#' 
setMethod("accepted_name", signature(taxlist="taxlist", ConceptID="numeric"),
        function(taxlist, ConceptID, show_traits=FALSE, ...) {
            AcceptedName <- taxlist@taxonRelations[
                    taxlist@taxonRelations$TaxonConceptID %in%
                            ConceptID,c("TaxonConceptID","AcceptedName")]
            for(i in c("TaxonName","AuthorName"))
                AcceptedName[ ,i] <- taxlist@taxonNames[
                        match(AcceptedName$AcceptedName,
                                taxlist@taxonNames$TaxonUsageID),i]
            colnames(AcceptedName)[2] <- "TaxonUsageID"
			AcceptedName <- merge(AcceptedName, taxlist@taxonRelations[ ,
							c("TaxonConceptID","ViewID","Level")], sort=FALSE)
			if(show_traits)
				AcceptedName <- merge(AcceptedName, taxlist@taxonTraits,
						sort=FALSE, all.x=TRUE)
			return(AcceptedName)
        }
)

#' @rdname accepted_name
#' 
#' @aliases accepted_name,taxlist,missing-method
#' 
setMethod("accepted_name", signature(taxlist="taxlist", ConceptID="missing"),
        function(taxlist, ...) {
            ConceptID <- taxlist@taxonRelations$TaxonConceptID
            return(accepted_name(taxlist, ConceptID, ...))
        }
)

#' @rdname accepted_name
#' 
#' @aliases accepted_name<-
#' 
#' @exportMethod accepted_name<-
#' 
setGeneric("accepted_name<-", function(taxlist, ConceptID, value)
            standardGeneric("accepted_name<-"))

#' @rdname accepted_name
#' 
#' @aliases accepted_name<-,taxlist,numeric,numeric-method
#' 
setReplaceMethod("accepted_name", signature(taxlist="taxlist",
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
                    "AcceptedName"] <- value
            return(taxlist)
        }
)

#' @rdname accepted_name
#' 
#' @aliases synonyms
#' 
#' @exportMethod synonyms
#' 
setGeneric("synonyms",
		function(taxlist, ConceptID, ...)
			standardGeneric("synonyms")
)

#' @rdname accepted_name
#' 
#' @aliases synonyms,taxlist,numeric-method
#' 
setMethod("synonyms", signature(taxlist="taxlist", ConceptID="numeric"),
		function(taxlist, ConceptID, ...) {
			Syn <- taxlist@taxonNames[taxlist@taxonNames$TaxonConceptID %in%
							ConceptID,c("TaxonUsageID","TaxonConceptID",
							"TaxonName","AuthorName")]
			## Syn$AcceptedName <- with(taxlist@taxonRelations,
			##         AcceptedName[match(Syn$TaxonConceptID, TaxonConceptID)])
			Syn$AcceptedName <- taxlist@taxonRelations$AcceptedName[
					match(Syn$TaxonConceptID,
							taxlist@taxonRelations$TaxonConceptID)]
			## Syn$AuthorAcceptedName <- with(taxlist@taxonNames,
			##         AuthorName[match(Syn$AcceptedName, TaxonUsageID)])
			Syn$AuthorAcceptedName <- taxlist@taxonNames$AuthorName[
					match(Syn$AcceptedName, taxlist@taxonNames$TaxonUsageID)]
			## Syn$AcceptedName <- with(taxlist@taxonNames,
			##         TaxonName[match(Syn$AcceptedName, TaxonUsageID)])
			Syn$AcceptedName <- taxlist@taxonNames$TaxonName[
					match(Syn$AcceptedName, taxlist@taxonNames$TaxonUsageID)]
			## Syn <- Syn[!Syn$TaxonUsageID %in% with(taxlist@taxonRelations,
			##                 AcceptedName[TaxonConceptID %in% ConceptID]), ]
			Syn <- Syn[!Syn$TaxonUsageID %in%
							taxlist@taxonRelations$AcceptedName[
									taxlist@taxonRelations$TaxonConceptID %in%
											ConceptID], ]
			return(Syn)
		}
)

#' @rdname accepted_name
#' 
#' @aliases synonyms,taxlist,missing-method
#' 
setMethod("synonyms", signature(taxlist="taxlist", ConceptID="missing"),
		function(taxlist, ...) {
			ConceptID <- taxlist@taxonRelations$TaxonConceptID
			return(synonyms(taxlist, ConceptID, ...))
		}
)

#' @rdname accepted_name
#' 
#' @aliases basionym
#' 
#' @exportMethod basionym
#' 
setGeneric("basionym",
		function(taxlist, ConceptID, ...)
			standardGeneric("basionym")
)

#' @rdname accepted_name
#' 
#' @aliases basionym,taxlist,numeric-method
#' 
setMethod("basionym", signature(taxlist="taxlist", ConceptID="numeric"),
		function(taxlist, ConceptID, ...) {
			Basionym <- taxlist@taxonRelations[match(ConceptID,
							taxlist@taxonRelations$TaxonConceptID),
					c("TaxonConceptID","Basionym")]
			## Basionym$BasionymName <- with(taxlist@taxonNames,
			##         TaxonName[match(Basionym$Basionym, TaxonUsageID)])
			Basionym$BasionymName <- taxlist@taxonNames$TaxonName[
					match(Basionym$Basionym, taxlist@taxonNames$TaxonUsageID)]
			## Basionym$BasionymAuthor <- with(taxlist@taxonNames,
			##         AuthorName[match(Basionym$Basionym, TaxonUsageID)])
			Basionym$BasionymAuthor <- taxlist@taxonNames$AuthorName[
					match(Basionym$Basionym, taxlist@taxonNames$TaxonUsageID)]
			return(Basionym)
		}
)

#' @rdname accepted_name
#' 
#' @aliases basionym,taxlist,missing-method
#' 
setMethod("basionym", signature(taxlist="taxlist", ConceptID="missing"),
		function(taxlist, ...) {
			ConceptID <- taxlist@taxonRelations$TaxonConceptID
			return(basionym(taxlist, ConceptID, ...))
		}
)

#' @rdname accepted_name
#' 
#' @aliases basionym<-
#' 
#' @exportMethod basionym<-
#' 
setGeneric("basionym<-", function(taxlist, ConceptID, value)
			standardGeneric("basionym<-"))

#' @rdname accepted_name
#' 
#' @aliases basionym<-,taxlist,numeric,numeric-method
#' 
setReplaceMethod("basionym", signature(taxlist="taxlist", ConceptID="numeric",
				value="numeric"),
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
