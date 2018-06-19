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
	underscores <- gsub("_", "", file2)
	underscores <- nchar(file2) - nchar(underscores)
	Name <- lapply(Name, function(x, y) {
				if(length(x) < (y + 3))
					x <- c(x, rep_len("0", y - length(x) + 3))
				return(x)
			}, y=underscores)
	OUT <- as.data.frame(do.call(rbind, Name), stringsAsFactors=FALSE)[,
			c((underscores + 2):(underscores + 3))]
	colnames(OUT) <- c("date","suffix")
	OUT$filename <- inFolder
	OUT <- OUT[nchar(OUT$date) == 10,]
	OUT$order <- c(1:nrow(OUT))
	OUT$date <- as.Date(OUT$date)
	OUT$suffix <- as.integer(OUT$suffix)
	OUT <- OUT[with(OUT, order(date, suffix)),]
	message(paste0("Loading file '", OUT$filename[nrow(OUT)], "' to session."))
	if(length(path) == 1 & path[1] == ".") load(OUT$filename[nrow(OUT)],
				envir=.GlobalEnv) else
		load(file.path(paste(path, collapse="/"), OUT$filename[nrow(OUT)]),
				envir=.GlobalEnv)
}
