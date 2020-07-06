#' Add a suffix when strings are identical
#' 
#' An integer will be added as suffix in `x` if an identical value is in `y`.
#' If a value with suffix is already in `y`, the function will search for the
#' highest suffix to avoid duplicated values.
#' 
#' @param x (`character` of length 1) The name to be compared.
#' @param y (`character`) Existing names, with or without suffixes.
#' @param sep (`character` of length 1) Symbol to be placed between the original
#'     name and the suffix.
#' 
#' @return A `character` value, either `x` or `x` with suffix if already in `y`.
#' 
#' @keywords internal
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

#' Sorting files with time stamp and suffix
#' 
#' When backups have been saved with a time stamp in the file's name and a
#' suffix, in the case of more than one backup in one day.
#' 
#' @param file A character value indicating the root of the backup's name. It
#'     may include also the path relative to the working directory when the
#'     backup is stored in a sub-folder.
#' @param fext A character value indicating the file extension (including the
#'     dot symbol).
#' 
#' @return A data frame including the sorted names of backup files from the
#' oldest to the newest.
#' 
#' @keywords internal
sort_backups <- function(file, f_timestamp="%Y-%m-%d", fext=".rda") {
	path <- "."
	if(grepl("/", file, fixed=TRUE)) {
		path <- strsplit(file, "/", fixed=TRUE)[[1]]
		file2 <- path[length(path)]
		path <- paste(path[-length(path)], collapse="/")
	} else if(grepl("\\", file, fixed=TRUE)) {
		path <- strsplit(file, "\\", fixed=TRUE)[[1]]
		file2 <- path[length(path)]
		path <- paste(path[-length(path)], collapse="/")
	} else file2 <- file
	inFolder <- list.files(path=path, pattern=fext)
	inFolder <- inFolder[grepl(file2, inFolder, fixed=TRUE)]
	if(length(inFolder) == 0)
		stop("The requested backup is missing in the working directory.")
	Name <- sub(fext, "", inFolder, fixed=TRUE)
	Name <- strsplit(Name, "_", fixed=TRUE)
	underscores <- gsub("_", "", file2)
	underscores <- nchar(file2) - nchar(underscores)
	Name <- lapply(Name, function(x, y) {
				if(length(x) < (y + 3))
					x <- c(x, rep_len("0", y - length(x) + 3))
				return(x)
			}, y=underscores)
	OUT <- as.data.frame(do.call(rbind, Name), stringsAsFactors=FALSE)[ ,
			c((underscores + 2):(underscores + 3))]
	colnames(OUT) <- c("date","suffix")
	OUT$path <- path
	OUT$filename <- inFolder
	OUT$date <- as.Date(strptime(OUT$date, f_timestamp))
	OUT <- OUT[!is.na(OUT$date), ]
	OUT$suffix <- as.integer(OUT$suffix)
	## OUT <- OUT[with(OUT, order(date, suffix)), ]
	OUT <- OUT[order(OUT$date, OUT$suffix), ]
	OUT$order <- c(seq_len(nrow(OUT)))
	return(OUT)
}

#' Filling missed columns with NAs
#' 
#' If columns of `y` are missed in `x`, the later gets these columns filled with
#' `NA` values.
#' 
#' @param x (`data.frame`) The data frame to be compared.
#' @param y (`data.frame`) The data frame used as reference.
#' 
#' @return A `data.frame`.
#' 
#' @keywords internal
add_nacolumn <- function(x, y) {
	for(i in y[!y %in% colnames(x)])
		x[ ,i] <- NA
	return(x)
}

#' Inserting new rows and columns by merging two data frames
#' 
#' Two data frames sharing partially columns will be merged including the sum
#' of all variables.
#' 
#' @param x,y (`data.frame`) The data frames to be merged.
#' 
#' @keywords internal
two2one_df <- function(x, y) {
	x <- add_nacolumn(x, colnames(y))
	y <- add_nacolumn(y, colnames(x))
	x <- do.call(rbind, list(x, y[ , colnames(x)]))
	return(x)
}
