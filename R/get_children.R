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
#' @example examples/get_children.R
#'
#' @rdname get_children
#'
#' @export
get_children <- function(taxlist, ...) {
  UseMethod("get_children", taxlist)
}

#' @rdname get_children
#' @aliases get_children,taxlist-method
#' @method get_children taxlist
#' @export
get_children.taxlist <- function(taxlist, ConceptID, ...) {
  if (is(ConceptID, "taxlist")) {
    ConceptID <- ConceptID@taxonRelations$TaxonConceptID
  }
  ConceptID <- list(ConceptID)
  repeat {
    if (!any(ConceptID[[length(ConceptID)]] %in%
      taxlist@taxonRelations$Parent)) {
      break
    }
    ConceptID[[length(ConceptID) + 1]] <- taxlist@taxonRelations[
      taxlist@taxonRelations$Parent %in%
        ConceptID[[length(ConceptID)]],
      "TaxonConceptID"
    ]
  }
  ConceptID <- do.call(c, ConceptID)
  taxlist@taxonRelations <- taxlist@taxonRelations[
    taxlist@taxonRelations$TaxonConceptID %in% ConceptID,
  ]
  return(clean(taxlist))
}

#' @rdname get_children
#' @aliases get_parents
#' @export
get_parents <- function(taxlist, ...) {
  UseMethod("get_parents", taxlist)
}

#' @rdname get_children
#' @aliases get_parents,taxlist-method
#' @method get_parents taxlist
#' @export
get_parents.taxlist <- function(taxlist, ConceptID, ...) {
  if (is(ConceptID, "taxlist")) {
    ConceptID <- ConceptID@taxonRelations$TaxonConceptID
  }
  ConceptID <- list(ConceptID)
  repeat {
    if (all(is.na(taxlist@taxonRelations[
      taxlist@taxonRelations$TaxonConceptID %in%
        ConceptID[[length(ConceptID)]],
      "Parent"
    ]))) {
      break
    }
    ConceptID[[length(ConceptID) + 1]] <- taxlist@taxonRelations[
      taxlist@taxonRelations$TaxonConceptID %in%
        ConceptID[[length(ConceptID)]], "Parent"
    ]
  }
  ConceptID <- do.call(c, ConceptID)
  ConceptID <- na.omit(ConceptID)
  taxlist@taxonRelations <- taxlist@taxonRelations[
    taxlist@taxonRelations$TaxonConceptID %in% ConceptID,
  ]
  return(clean(taxlist))
}
