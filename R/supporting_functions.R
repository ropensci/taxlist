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

# Filling missed columns with NAs
# x is a data frame
# y is a character vector to be compared with names of columns in x
add_nacolumn <- function(x, y) {
	for(i in y[!y %in% colnames(x)])
		x[,i] <- NA
	return(x)
}

# Inserting new rows and columns by merging two data frames
# x is a data frame
# y is a data frame
two2one_df <- function(x, y) {
	x <- add_nacolumn(x, colnames(y))
	y <- add_nacolumn(y, colnames(x))
	x <- do.call(rbind, list(x, y[, colnames(x)]))
	return(x)
}



