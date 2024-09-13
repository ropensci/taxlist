#' @name reindex
#'
#' @title Re-index elements of taxlist objects
#'
#' @description
#' The assignment of new identifiers must take into account all possible
#' occurrences of such indices in [taxlist-class] objects in order to maintain
#' their validity.
#'
#' @param object A [taxlist-class] object.
#' @param old A vector with old identifiers to be re-indized. This may contain
#'     all identifiers or only a part of them. If only a part, the rest of
#'     indices will be preserved. If the changes insert duplicated identifiers,
#'     an error message will be retrieved. If missing, all identifiers in
#'     `'object'` will be considered.
#' @param new A vector with the new identifiers. It has to be of the same length
#'     as `'old'`.
#' @param idx Name of the index to be changed, which means `"TaxonConceptID"`,
#'     `"TaxonUsageID"`, or `"ViewID"` for taxon concepts, taxon usage names, or
#'     taxon views, respectively. You can also use the aliases `"concept"`,
#'     `"usage"`, and `"view"`.
#' @param ... Further arguments to be passed among methods.
#'
#' @return An object of class [taxlist-class] with modified identifiers.
#'
#' @rdname reindex
#'
#' @export
reindex <- function(object, ...) {
  UseMethod("reindex", object)
}

#' @rdname reindex
#' @aliases reindex,taxlist-method
#' @method reindex taxlist
#' @export
reindex.taxlist <- function(
    object, old, new,
    idx = "TaxonConceptID", ...) {
  # Match indices
  idx <- sub("^taxon", "", tolower(idx))
  idx_alt <- c("TaxonConceptID", "TaxonUsageID", "ViewID")
  idx <- pmatch(idx, sub("^taxon", "", tolower(idx_alt)))
  if (!idx %in% seq_along(idx_alt)) {
    stop("Invalid value provided for 'idx'.")
  }
  # Depending on identifier
  if (idx == 1) {
    if (missing(old)) {
      old <- object@taxonRelations$TaxonConceptID
    }
    new_idx <- replace_x(object@taxonRelations$TaxonConceptID,
      old = old, new = new
    )
    if (any(duplicated(new_idx))) {
      stop("New index is causing duplicated values.")
    }
    # Replacing IDs
    object@taxonRelations$TaxonConceptID <- replace_x(
      object@taxonRelations$TaxonConceptID,
      old = object@taxonRelations$TaxonConceptID,
      new = new_idx
    )
    object@taxonRelations$Parent <- replace_x(
      object@taxonRelations$Parent,
      old = object@taxonRelations$TaxonConceptID,
      new = new_idx
    )
    object@taxonNames$TaxonConceptID <- replace_x(
      object@taxonNames$TaxonConceptID,
      old = object@taxonRelations$TaxonConceptID,
      new = new_idx
    )
    object@taxonTraits$TaxonConceptID <- replace_x(
      object@taxonTraits$TaxonConceptID,
      old = object@taxonRelations$TaxonConceptID,
      new = new_idx
    )
  }
  if (idx == 2) {
    if (missing(old)) {
      old <- object@taxonNames$TaxonUsageID
    }
    new_idx <- replace_x(object@taxonNames$TaxonUsageID,
      old = old, new = new
    )
    if (any(duplicated(new_idx))) {
      stop("New index is causing duplicated values.")
    }
    # Replacing IDs
    object@taxonNames$TaxonUsageID <- replace_x(
      object@taxonNames$TaxonUsageID,
      old = object@taxonNames$TaxonUsageID,
      new = new_idx
    )
    object@taxonRelations$AcceptedName <- replace_x(
      object@taxonRelations$AcceptedName,
      old = object@taxonNames$TaxonUsageID,
      new = new_idx
    )
    object@taxonRelations$Basionym <- replace_x(
      object@taxonRelations$Basionym,
      old = object@taxonNames$TaxonUsageID,
      new = new_idx
    )
  }
  if (idx == 3) {
    if (missing(old)) {
      old <- object@taxonViews$ViewID
    }
    new_idx <- replace_x(object@taxonViews$ViewID,
      old = old, new = new
    )
    if (any(duplicated(new_idx))) {
      stop("New index is causing duplicated values.")
    }
    # Replacing IDs
    object@taxonViews$ViewID <- replace_x(
      object@taxonViews$ViewID,
      old = object@taxonViews$ViewID,
      new = new_idx
    )
    object@taxonRelations$ViewID <- replace_x(
      object@taxonRelations$ViewID,
      old = object@taxonViews$ViewID,
      new = new_idx
    )
  }
  return(object)
}
