# TODO:   Some functions for data manipulation
# 
# Author: Miguel Alvarez
################################################################################

# Replace by value
replace_x <- function(x, old, new) {
	x[x %in% old] <- new[match(x[x %in% old], old)]
	return(x)
}
# Example
replace_x(letters, c("b", "p", "f"), c("bee", "pork", "fungus"))

# Replace by index
replace_idx <- function(x, idx1, idx2, new) {
	x[idx1 %in% idx2] <- new[match(idx1[idx1 %in% idx2], idx2)]
	return(x)
}
# Example
replace_x2(letters, 1:length(letters), c(2,7,17), c("second","seven",
				"seventeenth"))

# Replace if NA
replace_na

# Insert rows and columns in tables
insert_df

