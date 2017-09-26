# TODO:   Functions not exported to NAMESPACE
# 
# Author: Miguel Alvarez
################################################################################

# Adding suffixes when names are identical
# x is a character value
# y is a charcter vector
add_suffix <- function(x, y, sep="_") {
	if(x %in% y) {
		i <- 0
		repeat{
			i <- i + 1
			if(paste(x, i, sep=sep) %in% y) next else break
		}
		x <- paste(x, i, sep=sep)
	}
	return(x)
}
