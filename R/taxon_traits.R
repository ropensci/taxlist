#' @name taxon_traits
#'
#' @title Manipulation of taxon traits in taxlist objects.
#'
#' @description
#' The slot `taxonTraits` in [taxlist-class] objects contains
#' attributes of taxon concepts (e.g. functional traits).
#' These functions are suitable for replacing, retrieving and appending trait
#' information in taxonomic lists.
#'
#' @param taxlist A [taxlist-class] object.
#' @param taxonTraits a data frame with taxon traits to be inserted in
#'     `'taxlist'`. A column `'TaxonConceptID'` is mandatory in this table.
#'     If some taxon concept IDs are not occurring in `'taxlist'`, an error
#'     message is retrieved by `update_trait()`.
#' @param ConceptID Deprecated.
#' @param value Data frame to be set as slot `taxonTraits`.
#' @param ... Further arguments to be passed among methods.
#'
#' @details
#' Taxon traits are contained in a data frame at the slot `taxonTraits` in
#' [taxlist-class] objects.
#' To optimise space, this data frame contain only entries for those concepts
#' with information, while taxa with no information are skipped from this table.
#' Thus appending new variables may also have to include new rows in this slot,
#' which is automatically carried out by this function.
#'
#' The replacement method `taxon_traits<-` should be only used when
#' constructing [taxlist-class] objects from an empty one.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @seealso [taxlist-class].
#'
#' @example examples/taxon_traits.R
#'
#' @rdname taxon_traits
#'
#' @export
taxon_traits <- function(taxlist, ...) UseMethod("taxon_traits", taxlist)

#' @rdname taxon_traits
#' @aliases taxon_traits,taxlist-method
#' @method taxon_traits taxlist
#' @export
taxon_traits.taxlist <- function(taxlist, ...) taxlist@taxonTraits

#' @rdname taxon_traits
#' @aliases taxon_traits<-
#' @export
`taxon_traits<-` <- function(taxlist, ..., value) {
  UseMethod("taxon_traits<-", taxlist)
}

#' @rdname taxon_traits
#' @aliases taxon_traits<-,taxlist-method
#' @method taxon_traits<- taxlist
#' @export
`taxon_traits<-.taxlist` <- function(taxlist, ..., value) {
  if (!"TaxonConceptID" %in% names(value)) {
    stop("'TaxonConceptID' is a mandatory column in 'value'")
  }
  concept_ids <- value$TaxonConceptID[!value$TaxonConceptID %in%
    taxlist@taxonRelations$TaxonConceptID]
  if (length(concept_ids) > 0) {
    stop(paste0(
      "Following taxon concept IDs in 'value' are not ",
      "present in 'taxlist': '",
      paste0(concept_ids, collapse = "', '"), "'."
    ))
  }
  taxlist@taxonTraits <- value
  return(taxlist)
}

#' @rdname taxon_traits
#' @aliases update_trait
#' @export
update_trait <- function(taxlist, ...) UseMethod("update_trait", taxlist)

#' @rdname taxon_traits
#' @aliases update_trait,taxlist-method
#' @method update_trait taxlist
#' @export
update_trait.taxlist <- function(taxlist, taxonTraits, ConceptID, ...) {
  if (!is(taxonTraits, "data.frame")) {
    stop("Argument 'taxonTraits' has to be a data frame.")
  }
  if (!"TaxonConceptID" %in% names(taxonTraits)) {
    stop("'TaxonConceptID' is a mandatory column in 'taxonTraits'")
  }
  concept_ids <- taxonTraits$TaxonConceptID[!taxonTraits$TaxonConceptID %in%
    taxlist@taxonRelations$TaxonConceptID]
  if (length(concept_ids) > 0) {
    stop(paste0(
      "Following taxon concept IDs in 'taxonTraits' are not ",
      "in the 'taxlist' object: '",
      paste0(concept_ids, collapse = "', '"),
      "'."
    ))
  }
  taxlist@taxonTraits <- update_data(
    object = taxlist@taxonTraits,
    revision = taxonTraits, key = "TaxonConceptID", add = TRUE,
    update = TRUE
  )
  return(taxlist)
}
