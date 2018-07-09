# TODO:   Method cleaning character strings
# 
# Author: Miguel Alvarez
################################################################################

# Generic method
setGeneric("clean_strings",
		function(x, ...)
			standardGeneric("clean_strings"))

# Method for character
setMethod("clean_strings", signature(x="character"),
		function(x, from="utf8", to="utf8") {
			x <- iconv(x, from, to)
			x <- trimws(x, "both")
			x <- gsub("\\s+", " ", x)
			return(x)
		}
)

# Method for factor
setMethod("clean_strings", signature(x="factor"),
		function(x, from="utf8", to="utf8") {
			base::levels(x) <- clean_strings(base::levels(x), from, to)
			return(x)
		}
)

# Method for data.frame
setMethod("clean_strings", signature(x="data.frame"),
		function(x, from="utf8", to="utf8") {
			for(i in colnames(x)) {
				if(is.character(x[,i]) | is.factor(x[,i]))
					x[,i] <- clean_strings(x[,i], from, to)
			}
			return(x)
		}
)
