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
#' @param new,value A vector with the new identifiers. It has to be of
#'     the same length as `'old'`.
#' @param idx Name of the index to be changed, which means `"TaxonConceptID"`,
#'     `"TaxonUsageID"`, or `"ViewID"` for taxon concepts, taxon usage names, or
#'     taxon views, respectively. You can also use the aliases `"concept"`,
#'     `"usage"`, and `"view"`.
#' @param ... Further arguments to be passed among methods.
#'
#' @return An object of class [taxlist-class] with modified identifiers.
#'
#' @example examples/reindex.R
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
    if (any(duplicated(replace_x(object@taxonRelations$TaxonConceptID,
      old = old, new = new
    )))) {
      stop("New index is causing duplicated values.")
    }
    # Replacing IDs
    object@taxonRelations$TaxonConceptID <- replace_x(
      object@taxonRelations$TaxonConceptID,
      old = old,
      new = new
    )
    object@taxonRelations$Parent <- replace_x(
      object@taxonRelations$Parent,
      old = old,
      new = new
    )
    object@taxonNames$TaxonConceptID <- replace_x(
      object@taxonNames$TaxonConceptID,
      old = old,
      new = new
    )
    object@taxonTraits$TaxonConceptID <- replace_x(
      object@taxonTraits$TaxonConceptID,
      old = old,
      new = new
    )
  }
  if (idx == 2) {
    if (missing(old)) {
      old <- object@taxonNames$TaxonUsageID
    }
    if (any(duplicated(replace_x(object@taxonNames$TaxonUsageID,
      old = old, new = new
    )))) {
      stop("New index is causing duplicated values.")
    }
    # Replacing IDs
    object@taxonNames$TaxonUsageID <- replace_x(
      object@taxonNames$TaxonUsageID,
      old = old,
      new = new
    )
    object@taxonRelations$AcceptedName <- replace_x(
      object@taxonRelations$AcceptedName,
      old = old,
      new = new
    )
    object@taxonRelations$Basionym <- replace_x(
      object@taxonRelations$Basionym,
      old = old,
      new = new
    )
  }
  if (idx == 3) {
    if (missing(old)) {
      old <- object@taxonViews$ViewID
    }
    if (any(duplicated(replace_x(object@taxonViews$ViewID,
      old = old, new = new
    )))) {
      stop("New index is causing duplicated values.")
    }
    # Replacing IDs
    object@taxonViews$ViewID <- replace_x(
      object@taxonViews$ViewID,
      old = old,
      new = new
    )
    object@taxonRelations$ViewID <- replace_x(
      object@taxonRelations$ViewID,
      old = old,
      new = new
    )
  }
  return(object)
}

#' @rdname reindex
#' @aliases reindex<-
#' @export
`reindex<-` <- function(object, ..., value) UseMethod("reindex<-", object)

#' @rdname reindex
#' @aliases reindex<-,taxlist-method
#' @method reindex<- taxlist
#' @export
`reindex<-.taxlist` <- function(object, ..., value) {
  return(reindex(object, new = value, ...))
}
