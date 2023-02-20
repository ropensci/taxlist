#' @name taxon_relations
#'
#' @title Retrieve or replace slot taxonRelations in taxlist objects
#'
#' @description
#' Retrieve the content of slot `taxonRelations` from a
#' [taxlist-class] object or replace it by a new data frame.
#'
#' @param taxlist A [taxlist-class] object.
#' @param value A `data.frame` object to be set as slot `taxonRelations`.
#' @param ConceptID Concept IDs to be updated.
#' @param ... Further arguments passed among methods.
#'
#' @details
#' The replacement method `taxon_relations<-` should be only used when
#' constructing [taxlist-class] objects from an empty one
#' (prototype).
#'
#' New concepts should be first added to a [taxlist-class] object
#' using their respective accepted names.
#' Synonyms can be further provided using the function [add_synonym()].
#'
#' Additional named vectors can be provided to be included in slot `taxonNames`,
#' in the cases where those variables already exist, otherwise they will be
#' ignored.
#'
#' It is recommended also to provide a concept view as `ViewID` (see
#' [taxon_views()]).
#' For adding a new view, use [add_view()].
#'
#' @return
#' An object of class [taxlist-class] with added names and
#' concepts.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @seealso [taxlist-class]
#'
#' @example examples/taxon_relations.R
#'
#' @rdname taxon_relations
#'
#' @export
taxon_relations <- function(taxlist, ...) UseMethod("taxon_relations", taxlist)

#' @rdname taxon_relations
#' @aliases taxon_relations,taxlist-method
#' @method taxon_relations taxlist
#' @export
taxon_relations.taxlist <- function(taxlist, ...) taxlist@taxonRelations

#' @rdname taxon_relations
#' @aliases taxon_relations<-
#' @export
`taxon_relations<-` <- function(taxlist, ..., value) {
  UseMethod("taxon_relations<-", taxlist)
}

#' @rdname taxon_relations
#' @aliases taxon_relations<-,taxlist-method
#' @method taxon_relations<- taxlist
#' @export
`taxon_relations<-.taxlist` <- function(taxlist, ..., value) {
  if (!is(value, "data.frame")) {
    stop("Argument 'value' have to be of class 'data.frame'.")
  }
  tab_names <- c("TaxonConceptID", "AcceptedName")
  tab_names <- tab_names[!tab_names %in% names(value)]
  if (length(tab_names) > 0) {
    stop(paste0(
      "Following mandatory names are missing in 'value': '",
      paste0(tab_names, collapse = "', '"), "'."
    ))
  }
  tab_names <- c("Basionym", "Parent", "Level", "ViewID")
  for (i in tab_names) {
    if (!i %in% names(value)) {
      value[, i] <- NA
    }
  }
  taxlist@taxonRelations <- value
  return(taxlist)
}
