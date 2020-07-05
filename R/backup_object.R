#' @name backup_object
#' 
#' @title Make and load backups of R objects
#' 
#' @description 
#' When work with data becomes risky, the best practice is to produce backup
#' files.
#' The function of `backup_object` is a wrapper of [save()], adding a
#' time stamp and a suffix to the name of the resulting file (an R image file
#' with extension \bold{*.rda}).
#' The function `load_last` is adapted to this style, loading the newest
#' version to the session.
#' 
#' @param ... Names of the objects to be saved (either symbols or character
#'     strings).
#' @param objects A character vector indicating the names of objects to be
#'     included in the backup file.
#' @param file A character value indicating the name of the backup file, without
#'     the extension.
#' @param stamp A logical value indicating whether time should be stamped in the
#'     backup name or not.
#' @param fext A character value indicating the file extension (including the
#'     dot symbol).
#' @param overwrite A logical value indicating whether existing files must be
#'     overwritten or not.
#' 
#' @details 
#' In both functions the argument `file` may include either the path
#' relative to the working directory or the absolute path to the file, excluding
#' stamps and extension.
#' For `overwrite=FALSE` (the default), a numeric suffix will be added to
#' the backup's name, if another backup was produced at the same day.
#' For `overwrite=TRUE` no suffix will be included in the file and existing
#' files will be overwritten.
#' 
#' The function `load_last()` will load the newest version among backups
#' stored in the same folder, provided that the backup name includes a time
#' stamp.
#' 
#' @return An R image with extension \bold{*.rda}.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @seealso \code{\link{save}} \code{\link{load}}.
#' 
#' @examples 
#' \dontrun{
#' ## A subset with Pseudognaphalium and relatives
#' Pseudognaphalium <- subset(x=Easplist, subset=grepl("Pseudognaphalium",
#'     TaxonName), slot="names")
#' Pseudognaphalium <- get_parents(Easplist, Pseudognaphalium)
#' 
#' ## Create a backup with date stamp
#' backup_object(Pseudognaphalium, file="Pseudonaphalium")
#' 
#' ## The same
#' backup_object(objects="Pseudognaphalium", file="Pseudonaphalium")
#' 
#' ## To load the last backup into a session
#' load_last("Pseudognaphalium")
#' }
#' 
#' ## Load pre-installed backup
#' load_last(file.path(path.package("taxlist"), "extdata", "Podocarpus"))
#' 
#' @rdname backup_object
#' 
#' @export
#'  
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

#' @rdname backup_object
#' 
#' @export 
#' 
load_last <-function(file, fext=".rda") {
	OUT <- sort_backups(file=file, fext=fext)
	message(paste0("Loading file '", OUT$filename[nrow(OUT)], "' to session."))
	path <- OUT$path[nrow(OUT)]
	if(length(path) == 1 & path[1] == ".") load(OUT$filename[nrow(OUT)],
				envir=.GlobalEnv) else
		load(file.path(paste(path, collapse="/"), OUT$filename[nrow(OUT)]),
				envir=.GlobalEnv)
}
