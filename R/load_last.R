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
	underscores <- gsub("_", "", file)
	underscores <- nchar(file) - nchar(underscores)
	N <- min(sapply(Name, length))
	Name <- lapply(Name, function(x, y) {
				if(length(x) == y)
					x <- c(x, "0")
				return(x)
			}, y=N)
	Name <- as.data.frame(do.call(rbind, Name),
			stringsAsFactors=FALSE)[,c(N,N + 1)]
	colnames(Name) <- c("date","suffix")
	Name$order <- c(1:nrow(Name))
	Name$date <- as.Date(Name$date)
	Name$suffix <- as.integer(Name$suffix)
	Name <- Name[with(Name, order(date, suffix)),]
	message(paste0("Loading file '", inFolder[Name[nrow(Name),"order"]],
					"' to session."))
	if(path == ".") load(inFolder[Name[nrow(Name),"order"]]) else
		load(file.path(path, inFolder[Name[nrow(Name),"order"]]), envir=.GlobalEnv)
}
