#' @name clean_strings
#' 
#' @title Cleaning character strings.
#' 
#' @description 
#' Multiple, leading and trailing white spaces as well as wrong encodings may
#' cause serious problems in information dealing with taxonomic names.
#' The function `clean_strings` get rid of them.
#' 
#' @param x Object to be cleaned.
#' @param from,to Arguments passed to [iconv()].
#' @param ... Further arguments passed among methods (not yet in use).
#' 
#' @details 
#' This function automatically deletes leading, trailing and multiple white
#' spaces, either in strings (method `character`), levels (method
#' `factor` or in single columns (method `data.frame`).
#' 
#' @return The same as input `x`.
#' 
#' @author Miguel Alvarez.
#' 
#' @examples  
#' library(taxlist)
#' clean_strings(" Cyperus    papyrus L.     ")
#' 
#' @rdname clean_strings
#' 
#' @exportMethod clean_strings
#' 
setGeneric("clean_strings",
		function(x, ...)
			standardGeneric("clean_strings"))

#' @rdname clean_strings
#' 
#' @aliases clean_strings,character-method
#' 
setMethod("clean_strings", signature(x="character"),
		function(x, from="utf8", to="utf8", ...) {
			x <- iconv(x, from, to)
			x <- trimws(x, "both")
			x <- gsub("\\s+", " ", x)
			return(x)
		}
)

#' @rdname clean_strings
#' 
#' @aliases clean_strings,factor-method
#' 
setMethod("clean_strings", signature(x="factor"),
		function(x, from="utf8", to="utf8", ...) {
			base::levels(x) <- clean_strings(base::levels(x), from, to, ...)
			return(x)
		}
)

#' @rdname clean_strings
#' 
#' @aliases clean_strings,data.frame-method
#' 
setMethod("clean_strings", signature(x="data.frame"),
		function(x, from="utf8", to="utf8", ...) {
			for(i in colnames(x)) {
				if(is.character(x[ ,i]) | is.factor(x[ ,i]))
					x[ ,i] <- clean_strings(x[ ,i], from, to)
			}
			return(x)
		}
)
