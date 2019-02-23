# TODO:   Some functions for data manipulation
# 
# Author: Miguel Alvarez
################################################################################

# Replace by value
replace_x <- function(x, old, new) {
	if(length(old) != length(new))
		stop("Arguments 'old' and 'new' have to be of the same length.")
	x[x %in% old] <- new[match(x[x %in% old], old)]
	return(x)
}

# Replace by index
replace_idx <- function(x, idx1, idx2, new) {
	if(length(x) != length(idx1))
		stop("Arguments 'x' and 'idx1' have to be of the same length.")
	if(length(idx2) != length(new))
		stop("Arguments 'idx2' and 'new' have to be of the same length.")
	x[idx1 %in% idx2] <- new[match(idx1[idx1 %in% idx2], idx2)]
	return(x)
}

# Replace if NA
replace_na <- function(x, idx1, idx2, new) {
	if(length(x) != length(idx1))
		stop("Arguments 'x' and 'idx1' have to be of the same length.")
	if(length(idx2) != length(new))
		stop("Arguments 'idx2' and 'new' have to be of the same length.")
	x[idx1 %in% idx2 & is.na(x)] <- new[match(idx1[idx1 %in% idx2 & is.na(x)],
					idx2)]
	return(x)
}

# Insert rows and columns in tables
insert_rows <- function(x, y) {
	for(i in colnames(y)[!colnames(y) %in% colnames(x)])
		x[,i] <- NA
	for(i in colnames(x)[!colnames(x) %in% colnames(y)])
		y[,i] <- NA
	x <- do.call(rbind, list(x, y[,colnames(x)]))
	return(x)
}

