#' @name merge_taxa
#'
#' @title Merge concepts or move names
#'
#' @description
#' Merge taxon concepts form a [taxlist-class] object into single ones.
#'
#' @param object,taxlist Object of class [taxlist-class].
#' @param concepts Numeric (integer) vector including taxon concepts to be
#'     merged.
#' @param level Character vector with queried taxonomic ranks. This setting
#'     works only if `concepts` are missing. ranks in between will be merged to
#'     their respective parents by [merge_to_parent()]. Non queried ranks as
#'     well as rankless concepts will be deleted from the output.
#' @param print_output Logical value indicating whether the merged concept
#'     should be displayed in the console.
#' @param ... Further arguments to be passed to or from other methods.
#'
#' @details
#' Taxon concepts indicated in argument `concepts` will be merged into a
#' single concept.
#' The new concept inherits the ID and respective attributes from slots
#' `taxonRelations` and `taxonTraits` from the first taxon concept
#' indicated in argument `concepts`.
#'
#' For convenience the resulting concept can be displayed by setting
#' `print_output=TRUE` but only when using argument `concepts`.
#'
#' An alternative application of this function is implemented through the
#' argument `level`, where all lower rank taxa will be merged to the indicated
#' level or higher (if parent of merged taxa are at a higher rank).
#'
#' @return An object of class [taxlist-class].
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @example examples/merge_taxa.R
#'
#' @rdname merge_taxa
#'
#' @export
merge_taxa <- function(object, ...) {
  UseMethod("merge_taxa", object)
}

#' @rdname merge_taxa
#' @aliases merge_taxa,taxlist-method
#' @method merge_taxa taxlist
#' @export
merge_taxa.taxlist <- function(
    object, concepts, level = NULL, print_output = FALSE,
    ...) {
  if (!missing(concepts)) {
    # Tests previous running function
    if (!length(concepts) > 1) {
      stop("Argument 'concepts' must have a length > 1")
    }
    if (any(!concepts %in% object@taxonRelations$TaxonConceptID)) {
      stop(paste(
        "All values in 'concepts' should be included as",
        "concepts in 'object'"
      ))
    }
    # Merging concepts
    object@taxonNames[
      object@taxonNames$TaxonConceptID %in% concepts,
      "TaxonConceptID"
    ] <- concepts[1]
    object@taxonRelations[
      object@taxonRelations$Parent %in% concepts,
      "Parent"
    ] <- concepts[1]
    object@taxonRelations <- object@taxonRelations[
      !object@taxonRelations$TaxonConceptID %in% concepts[-1],
    ]
    object <- clean(object)
    # Print result#
    if (print_output) {
      summary(object, concepts[1])
    }
  } else {
    level <- level[level %in% levels(object)]
    if (!length(level)) {
      stop(paste(
        "The queried ranks in 'level' are not included as levels",
        "in 'object'"
      ))
    }
    for (i in levels(object)) {
      sel_level <- object@taxonRelations$TaxonConceptID[
        object@taxonRelations$Level == i
      ]
      if (i %in% level) next
      object <- merge_to_parent(object, concept_id = sel_level)
    }
    # Select only selected levels
    object@taxonRelations <- object@taxonRelations[
      object@taxonRelations$Level %in% level,
    ]
    object <- clean(object)
  }
  # Return modified object
  return(object)
}
