# TODO:   A function dissecting scientific names into their parts
# 
# Author: Miguel Alvarez
################################################################################

dissect_name <- function(x, split=" ", fixed=TRUE, ...) {
	x <- strsplit(x, split=split, fixed=fixed, ...)
	LEN <- sapply(x, length)
	Expand <- function(y, z) c(y, rep(NA, z - length(y)))
	x <- t(sapply(x, Expand, z=max(LEN)))
	return(x)
}
