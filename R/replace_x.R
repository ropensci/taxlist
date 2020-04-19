#' @name replace_x
#' 
#' @title Data manipulation.
#' 
#' @description
#' Functions provided for fast replacement and update of data.
#' 
#' @param x A vector to be modified or a data frame in the case of
#'     `insert_rows`.
#' @param old,new Vectors containing the values to be replaced and the updated
#'     values, respectively.
#' @param idx1,idx2 Indices applied for the values in 'x' and the values to be
#'     replaced, respectively.
#' @param y Data frame including rows to be inserted in `x`.
#' 
#' @details 
#' These are functions implemented for efficient coding of insert and replace
#' routines.
#' 
#' The functions `replace_x` and `replace_idx` replace values in vectors,
#' in the first case comparing values in the vector and in the second one by
#' using indices.
#' The function `replace_na` works in the same way as `replace_idx`, but
#' carries out the replacement only if the previous value is a `NA`.
#' 
#' The function `insert_rows` inserts `y` as new rows in `x`. If `y` contains
#' columns absent in `x`, they will be added to the output data frame.
#' 
#' @return A vector or data frame with the modified values.
#' 
#' @author Miguel Alvarez.
#' 
#' @examples 
#' library(taxlist)
#' 
#' ## Replace values in vector
#' replace_x(letters, c("b", "p", "f"), c("bee", "pork", "fungus"))
#' 
#' ## Replace values using indices
#' replace_idx(letters, 1:length(letters), c(2,7,17), c("second", "seven",
#'     "seventeenth"))
#' 
#' ## Replace values if they are NAs
#' letters[2] <- NA
#' replace_na(letters, 1:length(letters), c(1:3), c("alpha", "beta", "zeta"))
#' 
#' ## Merge data frames including new columns
#' data(iris)
#' iris$Species <- paste(iris$Species)
#' new_iris <- data.frame(Species=rep("humilis", 2), Height=c(15,20),
#'     stringsAsFactors=FALSE)
#' insert_rows(iris, new_iris)
#' 
#' @rdname replace_x
#' @export 
replace_x <- function(x, old, new) {
	if(length(old) != length(new))
		stop("Arguments 'old' and 'new' have to be of the same length.")
	x[x %in% old] <- new[match(x[x %in% old], old)]
	return(x)
}

#' @rdname replace_x
#' 
#' @aliases replace_idx
#' 
#' @export
#' 
replace_idx <- function(x, idx1, idx2, new) {
	if(length(x) != length(idx1))
		stop("Arguments 'x' and 'idx1' have to be of the same length.")
	if(missing(idx2)) idx2 <- idx1
	if(length(idx2) != length(new))
		stop("Arguments 'idx2' and 'new' have to be of the same length.")
	x[idx1 %in% idx2] <- new[match(idx1[idx1 %in% idx2], idx2)]
	return(x)
}

#' @rdname replace_x
#' 
#' @aliases replace_na
#' 
#' @export
#' 
replace_na <- function(x, idx1, idx2, new) {
	if(length(x) != length(idx1))
		stop("Arguments 'x' and 'idx1' have to be of the same length.")
	if(missing(idx2)) idx2 <- idx1
	if(length(idx2) != length(new))
		stop("Arguments 'idx2' and 'new' have to be of the same length.")
	x[idx1 %in% idx2 & is.na(x)] <- new[match(idx1[idx1 %in% idx2 & is.na(x)],
					idx2)]
	return(x)
}

#' @rdname replace_x
#' 
#' @aliases insert_rows
#' 
#' @export 
#' 
insert_rows <- function(x, y) {
	for(i in colnames(y)[!colnames(y) %in% colnames(x)])
		x[,i] <- NA
	for(i in colnames(x)[!colnames(x) %in% colnames(y)])
		y[,i] <- NA
	x <- do.call(rbind, list(x, y[,colnames(x)]))
	return(x)
}
