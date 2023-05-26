#' @name merge_to_parent
#'
#' @title Merge taxa to their respective parents
#'
#' @description
#' Aggregation of taxon concepts to their respective parents.
#' All names of aggregated concepts will become synonyms in the target parent.
#'
#' @param object An object of class [taxlist-class].
#' @param concept_id A vector of IDs (TaxonConceptID) of taxa that will be
#'     aggregated into their respective parents. Note that if one of the IDs
#'     is simultaneously the parent of another ID in the vector, this function
#'     will retrieve an error message.
#' @param ... Further arguments passed among methods.
#'
#' @return An object of class [taxlist-class] with merged taxa.
#'
#' @seealso [merge_taxa()]
#'
#' @example examples/merge_to_parent.R
#'
#' @rdname merge_to_parent
#'
#' @export
merge_to_parent <- function(object, ...) {
  UseMethod("merge_to_parent", object)
}

#' @rdname merge_to_parent
#' @aliases merge_to_parent,taxlist-method
#' @method merge_to_parent taxlist
#' @export
merge_to_parent.taxlist <- function(object, concept_id, ...) {
  all_parents <- object@taxonRelations$Parent[
    object@taxonRelations$TaxonConceptID %in% concept_id
  ]
  err_parents <- concept_id[concept_id %in% all_parents]
  if (length(err_parents)) {
    stop(paste0(
      "Following concepts in 'concept_id' are included as parents: ",
      paste0(err_parents, collapse = ", "), "."
    ))
  }
  # in case of NA parents
  concept_id <- concept_id[!is.na(all_parents)]
  all_parents <- all_parents[!is.na(all_parents)]
  # assign names
  object@taxonNames$TaxonConceptID <-
    replace_x(object@taxonNames$TaxonConceptID,
      old = concept_id,
      new = all_parents
    )
  # change parents
  object@taxonRelations$Parent <- replace_x(object@taxonRelations$Parent,
    old = concept_id, new = all_parents
  )
  # delete old concepts
  object@taxonRelations <-
    object@taxonRelations[!object@taxonRelations$TaxonConceptID %in%
      concept_id, ]
  # clean object
  object <- clean(object)
  # output
  return(object)
}
