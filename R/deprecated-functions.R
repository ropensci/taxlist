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
add_parent <- function() {
  .Deprecated(
    new = "update_concept", package = "taxlist",
    msg = "'add_parent' is deprecated. Use 'update_concept' instead."
  )
}

#' @rdname Deprecated-functions
#' @aliases add_trait
#' @export add_trait
add_trait <- function() {
  .Deprecated(
    new = "update_trait", package = "taxlist",
    msg = "'add_trait' is deprecated. Use 'update_trait' instead."
  )
}

#' @rdname Deprecated-functions
#' @aliases add_level
#' @export add_level
add_level <- function() {
  .Deprecated(
    package = "taxlist",
    msg = paste(
      "'add_level' is deprecated. Use 'levels' or",
      "'update_concept' instead."
    )
  )
}

#' @rdname Deprecated-functions
#' @aliases replace_view
#' @export replace_view
replace_view <- function() {
  .Deprecated(
    new = "update_concept", package = "taxlist",
    msg = paste(
      "'replace_view' is deprecated. Use",
      "'update_concept' instead."
    )
  )
}

#' @rdname Deprecated-functions
#' @aliases taxlist2taxmap
#' @export taxlist2taxmap
taxlist2taxmap <- function() {
  .Deprecated(
    package = "taxlist",
    msg = paste(
      "This function is temporarily deprecated",
      "to avoid conflicts with new version of 'taxa'"
    )
  )
}

#' @rdname Deprecated-functions
#' @aliases taxmap2taxlist
#' @export taxmap2taxlist
taxmap2taxlist <- function() {
  .Deprecated(
    package = "taxlist",
    msg = paste(
      "This function is temporarily deprecated",
      "to avoid conflicts with new version of 'taxa'"
    )
  )
}

#' @rdname Deprecated-functions
#' @aliases taxmap2taxlist
#' @export taxmap2taxlist
taxmap2taxlist <- function() {
  .Deprecated(
    package = "taxlist",
    msg = paste(
      "This function is temporarily deprecated",
      "to avoid conflicts with new version of 'taxa'"
    )
  )
}

#' @rdname Deprecated-functions
#' @aliases tnrs
#' @export tnrs
tnrs <- function() {
  .Deprecated(
    package = "taxlist",
    msg = paste(
      "'tnrs' is deprecated due to changes in package 'taxize'.",
      "Check into the documentation of that package for alternatives."
    )
  )
}
