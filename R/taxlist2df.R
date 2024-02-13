#' @name taxlist2df
#'
#' @title Coerce taxlist objects to data frames
#'
#' @description
#' Transform [taxlist-class] objects into data frames.
#'
#' @param x A [taxlist-class] object to be coerced.
#' @param include_traits A logical value indicating whether taxon concept
#'     attributes have to be included in the output or not.
#' @param include_views A logical value indicating whether taxon views
#'     have to be included in the output or not.
#' @param ... Further arguments passed among methods.
#'
#' @rdname taxlist2df
#' @aliases taxlist2df
#' @export
taxlist2df <- function(x, ...) UseMethod("taxlist2df", x)

#' @rdname taxlist2df
#' @aliases taxlist2df,taxlist-method
#' @export
taxlist2df.taxlist <- function(
    x, include_traits = FALSE,
    include_views = FALSE, ...) {
  x <- as(x, "list")
  double_names <- names(x$taxonNames)[names(x$taxonNames) %in%
    names(x$taxonRelations) & names(x$taxonNames) != "TaxonConceptID"]
  if (length(double_names) > 0) {
    names(x$taxonNames) <- replace_x(names(x$taxonNames),
      old = double_names,
      new = paste("names", double_names, sep = ".")
    )
  }
  x_out <- merge(x$taxonNames, x$taxonRelations, all = TRUE)
  if (include_traits) {
    double_names <- names(x$taxonTraits)[names(x$taxonTraits) %in%
      names(x_out) & names(x$taxonTraits) != "TaxonConceptID"]
    if (length(double_names) > 0) {
      names(x$taxonTraits) <- replace_x(names(x$taxonTraits),
        old = double_names,
        new = paste("traits", double_names, sep = ".")
      )
    }
    x_out <- merge(x_out, x$taxonTraits, all = TRUE)
  }
  if (include_views) {
    double_names <- names(x$taxonViews)[names(x$taxonViews) %in%
      names(x_out) & names(x$taxonViews) != "ViewID"]
    if (length(double_names) > 0) {
      names(x$taxonViews) <- replace_x(names(x$taxonViews),
        old = double_names,
        new = paste("views", double_names, sep = ".")
      )
    }
    x_out <- merge(x_out, x$taxonViews, all = TRUE)
  }
  return(x_out)
}

#' @name as
#' @rdname coerce-methods
#' @aliases coerce,taxlist,data.frame-method
setAs(from = "taxlist", to = "data.frame", def = function(from) {
  return(taxlist2df(from, include_traits = TRUE, include_views = TRUE))
})
