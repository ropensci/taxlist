#' @name sort_taxa
#'
#' @title Sort taxa for further print
#'
#' @description
#' Sorting taxa in slot **taxonRelations** will be reflected in printed list
#' by functions such as [summary()] and [indented_list()].
#' This is a wrapper for [order()].
#'
#' @param object Object of class [taxlist-class].
#' @param by Character value indicating the column name in slot
#'     **taxonRelations** of `object` which will be used for the sorting.
#'     Additionally you can use `by = "TaxonName"` (the default) to get an
#'     alphabetical sorting by accepted names.
#' @param priority Optional vector with values to be set on top of the list.
#'     Its class have to match the class of the column `by`.
#' @param ... Further arguments passed to [order()].
#'
#' @return An object of class [taxlist-class] with sorted relations.
#'
#' @example examples/sort_taxa.R
#'
#' @rdname sort_taxa
#'
#' @export
sort_taxa <- function(object, ...) {
  UseMethod("sort_taxa", object)
}

#' @rdname sort_taxa
#' @aliases sort_taxa,taxlist-method
#' @method sort_taxa taxlist
#' @export
sort_taxa.taxlist <- function(object, by = "TaxonName", priority, ...) {
  df <- object@taxonRelations
  df$TaxonName <- object@taxonNames$TaxonName[match(
    df$AcceptedName,
    object@taxonNames$TaxonUsageID
  )]
  # Retain priorities
  if (!missing(priority)) {
    priority <- df$TaxonConceptID[match(priority, df[[by]])]
    priority <- priority[!is.na(priority)]
  } else {
    priority <- character(0)
  }
  object@taxonRelations <- object@taxonRelations[order(df[[by]], ...), ]
  if (length(priority)) {
    object@taxonRelations <- do.call(
      rbind,
      list(
        object@taxonRelations[match(
          priority,
          object@taxonRelations$TaxonConceptID
        ), ],
        object@taxonRelations[!object@taxonRelations$TaxonConceptID %in% priority, ]
      )
    )
  }
  return(object)
}
