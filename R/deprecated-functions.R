#' @name Deprecated-functions
#' @aliases add_parent
#' @rdname Deprecated-functions
#' 
#' @title Deprecated functions
#' 
#' @description 
#' Most of those functions have been replaced by alternative 'update' ones.
#'
#' @export add_parent
#' 
add_parent <- function() {
	.Deprecated(msg="'add_parent' is deprecated. Use 'update_concept' instead.")
}

#' @rdname Deprecated-functions
#' 
#' @aliases add_trait
#' 
#' @export add_trait
#' 
add_trait <- function() {
	.Deprecated(msg="'add_trait' is deprecated. Use 'update_trait' instead.")
}

#' @rdname Deprecated-functions
#' 
#' @aliases add_level
#' 
#' @export add_level
#' 
add_level <- function() {
	.Deprecated(msg=paste("'add_level' is deprecated. Use 'levels' or",
					"'update_concept' instead."))
}

#' @rdname Deprecated-functions
#' 
#' @aliases replace_view
#' 
#' @export replace_view
#' 
replace_view <- function() {
	.Deprecated(msg=paste("'replace_view' is deprecated. Use",
					"'update_concept' instead."))
}
