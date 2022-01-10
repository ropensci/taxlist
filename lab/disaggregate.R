# TODO:   Candidate for taxlist
# 
# Author: Miguel Alvarez
###############################################################################

# disaggregate (this function will be developed in taxlist)
disaggregate <- function(data, id, var, sep, ...) {
	# Test for missing information
	if(any(is.na(data[ , id])))
		stop(paste0("NAs are not allowed in variable '", id, "'"))
	if(any(is.na(data[ , var])))
		stop(paste0("NAs are not allowed in variable '", var, "'"))
	# Unique values as id
	if(any(duplicated(data[ , id])))
		stop(paste0("Only unique values are allowed in variable '", id, "'"))
	# to character
	data2 <- data[ , c(id, var)]
	data2[ , id] <- paste(data2[ , id])
	data2[ , var] <- paste(data2[ , var])
	# disaggregate
	data2 <- mapply(cbind, split(data2[ , id], 1:nrow(data2)),
			strsplit(data2[ , var], sep, fixed = TRUE))
	data2 <- as.data.frame(do.call(rbind, data2), stringsAsFactors = FALSE)
	colnames(data2) <- c(id, var)
	data2[ , id] <- as(data2[ , id], class(data[ , id]))
	data2[ , var] <- as(data2[ , var], class(data[ , var]))
	# restore data
	for(i in colnames(data)[!colnames(data) %in% c(id, var)])
		data2[ , i] <- data[match(data2[ , id], data[ , id]), i]
	return(data2[ , colnames(data)])
}
