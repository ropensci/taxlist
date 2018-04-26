# TODO:   Function written to load the last backup of an object
# 
# Author: Miguel Alvarez
################################################################################

load_last <-function(file) {
	path <- "."
	if(grepl("/", file, fixed=TRUE)) {
		path <- strsplit(file, "/", fixed=TRUE)[[1]]
		file2 <- path[length(path)]
		path <- file.path(path[-length(path)])
	}
	if(grepl("\\", file, fixed=TRUE)) {
		path <- strsplit(file, "/", fixed=TRUE)[[1]]
		file2 <- path[length(path)]
		path <- file.path(path[-length(path)])
	}
	inFolder <- list.files(path=path, pattern=".rda")
	inFolder <- inFolder[grepl(file2, inFolder, fixed=TRUE)]
	if(length(inFolder) == 0)
		stop("The requested backup is missing in the working directory.")
	Name <- sub(".rda", "", inFolder, fixed=TRUE)
	Name <- strsplit(Name, "_", fixed=TRUE)
	step_back <- function(x) {
		if(length(x) > 2) x <- c(paste(x[1:(length(x) - 1)], collapse="_"),
					x[length(x)])
		return(x)
	}
	Name <- lapply(Name, step_back)
	Name[sapply(Name, function(x) length(x) == 2)] <- lapply(Name[sapply(Name,
							function(x) length(x) == 2)], function(x) c(x, "0"))
	Name <- as.data.frame(do.call(rbind, Name), stringsAsFactors=FALSE)
	colnames(Name) <- c("file","date","suffix")
	Name$order <- c(1:nrow(Name))
	Name$date <- as.Date(Name$date)
	Name$suffix <- as.integer(Name$suffix)
	Name <- Name[with(Name, order(date, suffix)),]
	message(paste0("Loading file '", inFolder[Name[nrow(Name),"order"]],
					"' to session."))
	if(path == ".") load(inFolder[Name[nrow(Name),"order"]]) else
		load(file.path(path, inFolder[Name[nrow(Name),"order"]]), envir=.GlobalEnv)
}
