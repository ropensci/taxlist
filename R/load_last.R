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
	Name <- lapply(Name, function(x, y) {
				if(length(x) == (y + 2))
					x <- c(x, "0")
				return(x)
			}, y=underscores)
	if(length(Name) == 1)
		OUT <- as.data.frame(rbind(Name[[1]]), stringsAsFactors=FALSE)[,
				c((underscores + 2):(underscores + 3))]
	if(length(Name) > 1)
		OUT <- as.data.frame(do.call(rbind, Name), stringsAsFactors=FALSE)[,
				c((underscores + 2):(underscores + 3))]
	colnames(OUT) <- c("date","suffix")
	OUT$order <- c(1:nrow(OUT))
	OUT$date <- as.Date(OUT$date)
	OUT$suffix <- as.integer(OUT$suffix)
	OUT <- OUT[with(OUT, order(date, suffix)),]
	message(paste0("Loading file '", inFolder[OUT[nrow(OUT),"order"]],
					"' to session."))
	if(path == ".") load(inFolder[OUT[nrow(OUT),"order"]]) else
		load(file.path(path, inFolder[OUT[nrow(OUT),"order"]]),
				envir=.GlobalEnv)
}
