#' @name dissect_name
#' 
#' @title Dissect Scientific Names into their Elements
#' 
#' @description 
#' Depending the degree of resolution and specific roles of nomenclature,
#' strings containing taxon usage names (scientific names) are constructed with
#' different parts.
#' A string with names can be consequently split into those elements, meanwhile
#' the number of elements will suggest the taxonomic ranks.
#' 
#' @param x A character vector containing taxon names.
#' @param split,fixed,... Arguments passed to [strsplit()].
#' 
#' @details 
#' This function is using [strsplit()] for splitting names.
#' Single spaces will be used to dissect names but it can be changed in the
#' value of argument `split`.
#' The number of columns in the resulting matrix will depend on the longest
#' polynomial string.
#' 
#' @return A character matrix with as many rows as names in the input vector.
#' 
#' @seealso [strsplit()]
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @examples 
#' Easplist <- subset(x=Easplist, subset=Level == "variety", slot="relations")
#' Easplist <- accepted_name(Easplist)[c(1:10),"TaxonName"]
#' 
#' dissect_name(Easplist)
#' 
#' @export 
#' 
dissect_name <- function(x, split=" ", fixed=TRUE, ...) {
	x <- strsplit(x, split=split, fixed=fixed, ...)
	LEN <- unlist(lapply(x, length))
	Expand <- function(y, z) c(y, rep(NA, z - length(y)))
	x <- do.call(rbind, lapply(x, Expand, z=max(LEN)))
	return(x)
}
