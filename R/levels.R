#' @name levels
#'
#' @title Set and retrieves hierarchical levels
#'
#' @description
#' Taxonomic hierarchies can be set as levels in [taxlist-class]
#' objects, ordered from lower to higher levels.
#'
#' Add taxonomic levels for specific taxon concepts in a
#' [taxlist-class] object.
#' Also changes in concept circumscription may implicate changes in its
#' taxonomic hierarchy.
#'
#' @param x A [taxlist-class] object.
#' @param value A character vector with replacement values for levels o `x`.
#'
#' @details
#' Taxonomic levels will be handled as factors in the
#' [taxlist-class] objects.
#' Those levels are useful for creating subsets of related groups (e.g. by
#' functions [get_children()] or [get_parents()]).
#'
#' Levels in combination to parent-child relationships will be further used for
#' checking consistency of taxonomic lists.
#'
#' A replacement method of the form `levels(x) <- value` it is also implemented.
#'
#' @return A `character` vector or a [taxlist-class] object with
#' added or modified taxonomic levels.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @example examples/levels.R
#'
#' @rdname levels
#'
#' @export
levels <- function(x) UseMethod("levels", x)

#' @rdname levels
#' @aliases levels,taxlist-method
#' @method levels taxlist
#' @export
levels.taxlist <- function(x) {
  if (all(is.na(x@taxonRelations$Level))) {
    stop("Taxonomic list without taxonomic ranks.")
  }
  base::levels(x@taxonRelations$Level)
}

#' @rdname levels
#' @aliases levels<-
#' @export
`levels<-` <- function(x, value) UseMethod("levels<-", x)

#' @rdname levels
#' @aliases levels<-,taxlist-method
#' @method levels<- taxlist
#' @export levels<-.taxlist
#' @export
`levels<-.taxlist` <- function(x, value) {
  if (!all(paste(x@taxonRelations$Level[
    !is.na(x@taxonRelations$Level)
  ]) %in% value)) {
    stop(paste(
      "Some levels are not matching those indicated in",
      "slot 'taxonRelations'"
    ))
  }
  x@taxonRelations$Level <- factor(
    paste(x@taxonRelations$Level),
    levels = value
  )
  return(x)
}
