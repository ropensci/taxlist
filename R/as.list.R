#' @name as.list
#' @aliases as.list,taxlist-method
#' @aliases S4_to_list
#' 
#' @title Coerce an S4 object to a list.
#' 
#' @description 
#' Coercion of S4 objects to lists can be applied to explore their content,
#' avoiding errors caused by their validation.
#' 
#' @param x An object of class \code{\linkS4class{taxlist}} or any S4 class.
#' @param ... further arguments passed to or from other methods.
#' 
#' @details 
#' The function `S4_to_list` transforms any S4 object to a list setting
#' slots to elements of the list and it is running internally in the method
#' `as.list` for \code{\linkS4class{taxlist}} objects.
#' 
#' @return An object of class \code{\link[base]{list}}.
#' 
#' @author Miguel Alvarez.
#' 
#' @examples 
#' library(taxlist)
#' data(Easplist)
#' Easplist <- as.list(Easplist)
#' class(Easplist)
#' 
#' @rdname as.list
#' @export 
S4_to_list <- function(x) {
    out <- list()
    for(i in slotNames(x)) out[[i]] <- slot(x, i)
    return(out)
}

#' @rdname as.list
#' 
#' @export 
setMethod("as.list", signature(x="taxlist"),
        function(x, ...) {
            S4_to_list(x)
        }
)
