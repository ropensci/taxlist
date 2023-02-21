#' @name as
#'
#' @title Coerce taxlist objects to lists.
#'
#' @description
#' Coercion of S4 objects to lists can be applied to explore their content,
#' avoiding errors caused by their validation.
#'
#' @param x An object of class [taxlist-class] or any S4 class.
#'
#' @details
#' Coerce [taxlist-class] objects to lists.
#'
#' @return An object of class [list].
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @example examples/coerce-methods.R
#'
#' @rdname coerce-methods
#' @aliases S4_to_list
#'
#' @export
S4_to_list <- function(x) {
  out <- list()
  for (i in slotNames(x)) out[[i]] <- slot(x, i)
  return(out)
}

#' @name as
#' @rdname coerce-methods
#' @aliases coerce,taxlist,list-method
setAs(from = "taxlist", to = "list", def = function(from) {
  return(S4_to_list(from))
})

#' @name as
#' @rdname coerce-methods
#' @aliases coerce,taxlist,data.frame-method
setAs(from = "taxlist", to = "data.frame", def = function(from) {
  DF <- merge(from@taxonRelations, from@taxonNames,
    by = "TaxonConceptID",
    all = TRUE, suffixes = c("", "@names")
  )
  DF <- merge(DF, from@taxonTraits,
    by = "TaxonConceptID",
    all = TRUE, suffixes = c("", "@traits")
  )
  DF <- merge(DF, from@taxonViews,
    by = "ViewID",
    all = TRUE, suffixes = c("", "@views")
  )
  return(DF)
})
