# TODO:   Backup option for R objects
# 
# Author: Miguel Alvarez
################################################################################

backup_object <- function(..., objects=character(), file, stamp=TRUE,
        overwrite=FALSE) {
    if(missing(file))
        stop("Missing value for argument 'file'")
	if(length(file) > 1) {
		file <- file[1]
		warning("Only the first element of argument 'file' will be used")
	}
	path <- "."
	file2 <- file
	if(grepl("/", file, fixed=TRUE)) {
		path <- strsplit(file, "/", fixed=TRUE)[[1]]
		file2 <- path[length(path)]
		path <- paste(path[-length(path)], collapse="/")
	}
	if(grepl("\\", file, fixed=TRUE)) {
		path <- strsplit(file, "\\", fixed=TRUE)[[1]]
		file2 <- path[length(path)]
		path <- paste(path[-length(path)], collapse="/")
	}
	inFolder <- list.files(path=path, pattern=".rda")
	if(stamp) stamp <- paste0("_", Sys.Date()) else stamp <- ""
    if(paste0(file2, stamp, ".rda") %in% inFolder & !overwrite) {
        i <- 0
        repeat{
            i <- i + 1
            if(paste0(file2, stamp, "_", i, ".rda") %in% inFolder) next
			else break
        }
        stamp <- paste(stamp, i, sep="_")
    }
	save(..., list=objects, file=paste0(file, stamp, ".rda"))
	message(paste0("Backup saved as '", file, stamp, ".rda'"))
}
