#' @name as.list
#'
#' @title Coerce taxlist objects to lists.
#'
#' @description
#' Coercion of S4 objects to lists can be applied to explore their content,
#' avoiding errors caused by their validation.
#'
#' @param x An object of class [taxlist-class] or any S4 class.
#' @param ... further arguments passed to or from other methods.
#'
#' @details
#' Coerce [taxlist-class] objects to lists.
#'
#' @return An object of class [list].
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @examples
#' Easplist <- as.list(Easplist)
#' class(Easplist)
#'
#' @rdname coerce-methods
#'
#' @aliases as.list
#'
#' @keywords internal
S4_to_list <- function(x) {
  out <- list()
  for (i in slotNames(x)) out[[i]] <- slot(x, i)
  return(out)
}

#' @rdname coerce-methods
#' @aliases as.list,taxlist-method
#' @exportMethod as.list
setMethod(
  "as.list", signature(x = "taxlist"),
  function(x, ...) {
    S4_to_list(x)
  }
)

setAs("taxlist", "list", function(from) as.list(from))

#' @rdname coerce-methods
#' @aliases as<-
#' @exportMethod as<-
setGeneric("as<-", function(from, value) {
  standardGeneric("as<-")
})

#' @rdname coerce-methods
#' @aliases as<-,taxlist-method
setReplaceMethod(
  "as", signature(from = "taxlist"),
  function(from, value) {
    from <- as(object = from, Class = value)
    return(from)
  }
)
