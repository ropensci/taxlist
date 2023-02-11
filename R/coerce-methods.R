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
setAs("taxlist", "list", function(from) {
  return(S4_to_list(from))
})
