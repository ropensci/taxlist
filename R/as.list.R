#' @name as.list
#' 
#' @title Coerce an S4 object to a list.
#' 
#' @description 
#' Coercion of S4 objects to lists can be applied to explore their content,
#' avoiding errors caused by their validation.
#' 
#' @param x An object of class [taxlist-class] or any S4 class.
#' @param ... further arguments passed to or from other methods.
#' 
#' @details 
#' The function `S4_to_list` transforms any S4 object to a list setting
#' slots to elements of the list and it is running internally in the method
#' `as.list` for [taxlist-class] objects.
#' 
#' @return An object of class [list].
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @examples 
#' Easplist <- as.list(Easplist)
#' class(Easplist)
#' 
#' @rdname as.list
#' 
#' @aliases S4_to_list
#' 
#' @export 
#' 
S4_to_list <- function(x) {
    out <- list()
    for(i in slotNames(x)) out[[i]] <- slot(x, i)
    return(out)
}

#' @rdname as.list
#' 
#' @aliases as.list,taxlist-method
#' 
#' @exportMethod as.list
#' 
setMethod("as.list", signature(x="taxlist"),
        function(x, ...) {
            S4_to_list(x)
        }
)
