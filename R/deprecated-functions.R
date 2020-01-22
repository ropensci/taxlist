#' Add parent to a taxon.
#'
#' @export
add_parent <- function() {
	.Deprecated(msg="'add_parent' is deprecated. Use 'update_concept' instead.")
}

#' Add trait to a taxon trait table.
#' 
#' @export 
add_trait <- function() {
	.Deprecated(msg="'add_trait' is deprecated. Use 'update_trait' instead.")
}

#' Add level information.
#'
#' @export 
add_level <- function() {
	.Deprecated(msg=paste("'add_level' is deprecated. Use 'levels' or",
					"'update_concept' instead."))
}

#' Replace taxon views in concepts.
#' 
#' @export 
replace_view <- function() {
	.Deprecated(msg=paste("'replace_view' is deprecated. Use",
					"'update_concept' instead."))
}
