#' @name as.list
#'
#' @title Coerce taxlist objects to lists.
#'
#' @description
#' Coercion of S4 objects to lists can be applied to explore their content,
#' avoiding errors caused by their validation.
#'
#' @param x An object of class [taxlist-class] or any S4 class.
#' @param object An object of class [taxlist-class] to be coerced.
#' @param Class A character value indicating the class for coercion.
#' @param value A character value indicating the class to be coerced to. Here
#'     it is always `"list"`.
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
#' @export
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

setAs("taxlist", "list", function(from) {
  as.list(from)
})

setGeneric("as<-", function(object, Class, value) {
  standardGeneric("as<-")
})

#' @rdname coerce-methods
#' @aliases as<- as<-,taxlist,missing,character-method
#' @exportMethod as<-
setMethod(
  "as<-", signature(object = "taxlist", Class = "missing", value = "character"),
  function(object, Class, value) {
    object <- as(object, value)
    return(object)
  }
)
