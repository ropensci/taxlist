#' @name prune_levels
#'
#' @title Prune not used taxonomic ranks
#'
#' @description
#' Taxonomic ranks without taxon concepts will be pruned in [taxlist-class]
#' objects.
#'
#' @param object An object of class [taxlist-class].
#' @param ... Further arguments passed among methods (not yet in use).
#'
#' @return An object of class [taxlist-class] with pruned taxonomic ranks.
#'
#' @example examples/prune_levels.R
#'
#' @rdname prune_levels
#'
#' @export
prune_levels <- function(object, ...) {
  UseMethod("prune_levels", object)
}

#' @rdname prune_levels
#' @aliases prune_levels,taxlist-method
#' @method prune_levels taxlist
#' @export
prune_levels.taxlist <- function(object, ...) {
  levels(object) <- levels(object)[levels(object) %in% as.character(object@taxonRelations$Level)]
  return(object)
}
