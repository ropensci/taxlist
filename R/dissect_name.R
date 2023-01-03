#' @name dissect_name
#'
#' @title Dissect Scientific Names into their Elements
#'
#' @description
#' Depending the degree of resolution and specific roles of nomenclature,
#' strings containing taxon usage names (scientific names) are constructed with
#' different parts.
#' A string with names can be consequently split into those elements, meanwhile
#' the number of elements may suggest the taxonomic ranks.
#'
#' This function is a wrapper of [strsplit()], while name element can be
#' re-pasted if indicated in argument `repaste`.
#'
#' @param x A character vector containing taxon names.
#' @param split,fixed,... Arguments passed to [strsplit()].
#' @param repaste An integer vector indicating the elements of the name selected
#'     for the output.
#'
#' @return
#' A character matrix with as many rows as names in the input vector.
#' If `repaste` is indicated, then the output will be a character vector.
#'
#' @seealso [strsplit()]
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @example examples/dissect_name.R
#'
#' @export dissect_name
#'
dissect_name <- function(x, split = " ", fixed = TRUE, repaste, ...) {
  x <- strsplit(x, split = split, fixed = fixed, ...)
  LEN <- unlist(lapply(x, length))
  Expand <- function(y, z) c(y, rep(NA, z - length(y)))
  x <- do.call(rbind, lapply(x, Expand, z = max(LEN)))
  if (!missing(repaste)) {
    x <- apply(x[, repaste, drop = FALSE], 1, paste, collapse = split)
  }
  return(x)
}
