#' @name get_children
#' 
#' @title Retrieve children or parents of taxon concepts
#' 
#' @description 
#' Retrieve all children or all parents of a queried taxon concept.
#' 
#' @param taxlist A [taxlist-class] object.
#' @param ConceptID Concept IDs for selecting parents or children or a subset of
#'     `taxlist`.
#' @param ... Further arguments passed among methods.
#' 
#' @details 
#' This function produces subsets of [taxlist-class] objects
#' including all children or parents of queried taxon concepts.
#' Multiple concepts can be queried in these function.
#' The argument `ConceptID` can be a vector of concept IDs or a subset of
#' the input `taxlist` object.
#' 
#' @return A [taxlist-class] object with a subset including
#' requested concepts with children or parents.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @examples 
#' ## Subset with family Ebenaceae and children
#' Ebenaceae <- subset(Easplist, charmatch("Ebenaceae", TaxonName))
#' Ebenaceae <- get_children(Easplist, Ebenaceae)
#' 
#' summary(Ebenaceae)
#' summary(object=Ebenaceae, ConceptID="all", maxsum=100)
#' 
#' ## Get parents of Diospyros tricolor
#' Diostri <- subset(x=Easplist, subset=TaxonConceptID == 52403,
#'     slot="relations")
#' Diostri <- get_parents(Easplist, Diostri)
#' 
#' summary(Diostri)
#' summary(Diostri, "all")
#' 
#' @rdname get_children
#' 
#' @exportMethod get_children
#' 
setGeneric("get_children",
        function(taxlist, ConceptID, ...)
            standardGeneric("get_children")
)

#' @rdname get_children
#' 
#' @aliases get_children,taxlist,numeric-method
#' 
setMethod("get_children", signature(taxlist="taxlist", ConceptID="numeric"),
        function(taxlist, ConceptID, ...) {
            ConceptID <- list(ConceptID)
            repeat {
                if(!any(ConceptID[[length(ConceptID)]] %in%
                                taxlist@taxonRelations$Parent)) break
                ConceptID[[length(ConceptID) + 1]] <- taxlist@taxonRelations[
                        taxlist@taxonRelations$Parent %in%
                                ConceptID[[length(ConceptID)]],
                        "TaxonConceptID"]
            }
            ConceptID <- do.call(c, ConceptID)
            taxlist@taxonRelations <- taxlist@taxonRelations[
                    taxlist@taxonRelations$TaxonConceptID %in% ConceptID, ]
            return(clean(taxlist))
        }
)

#' @rdname get_children
#' 
#' @aliases get_children,taxlist,taxlist-method
#' 
setMethod("get_children", signature(taxlist="taxlist", ConceptID="taxlist"),
        function(taxlist, ConceptID, ...) {
            ConceptID <- ConceptID@taxonRelations$TaxonConceptID
            return(get_children(taxlist, ConceptID))
        }
)

#' @rdname get_children
#' 
#' @aliases get_parents
#' 
#' @exportMethod get_parents
#' 
setGeneric("get_parents",
        function(taxlist, ConceptID, ...)
            standardGeneric("get_parents")
)

#' @rdname get_children
#' 
#' @aliases get_parents,taxlist,numeric-method
#' 
setMethod("get_parents", signature(taxlist="taxlist", ConceptID="numeric"),
        function(taxlist, ConceptID, ...) {
            ConceptID <- list(ConceptID)
            repeat {
                if(all(is.na(taxlist@taxonRelations[
                                     taxlist@taxonRelations$TaxonConceptID %in%
                                                ConceptID[[length(ConceptID)]],
                                        "Parent"]))) break
                ConceptID[[length(ConceptID) + 1]] <- taxlist@taxonRelations[
                        taxlist@taxonRelations$TaxonConceptID %in%
                                ConceptID[[length(ConceptID)]], "Parent"]
            }
            ConceptID <- do.call(c, ConceptID)
            ConceptID <- na.omit(ConceptID)
            taxlist@taxonRelations <- taxlist@taxonRelations[
                    taxlist@taxonRelations$TaxonConceptID %in% ConceptID, ]
            return(clean(taxlist))
        }
)

#' @rdname get_children
#' 
#' @aliases get_parents,taxlist,taxlist-method
#' 
setMethod("get_parents", signature(taxlist="taxlist", ConceptID="taxlist"),
        function(taxlist, ConceptID, ...) {
            ConceptID <- ConceptID@taxonRelations$TaxonConceptID
            return(get_parents(taxlist, ConceptID))
        }
)
