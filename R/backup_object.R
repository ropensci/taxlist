# TODO:   Backup option for R objects
# 
# Author: Miguel Alvarez
################################################################################

backup_object <- function(..., objects=character(), file, stamp=TRUE,
        overwrite=FALSE) {
    if(missing(file))
        stop("Missing value for argument 'file'")
    inFolder <- list.files(pattern=".rda")
	if(stamp) stamp <- paste0("_", Sys.Date()) else stamp <- ""
    if(paste0(file, stamp, ".rda") %in% inFolder & !overwrite) {
        i <- 0
        repeat{
            i <- i + 1
            if(paste0(file, i, stamp, ".rda") %in% inFolder) next
            if(!paste0(file, i, stamp, ".rda") %in% inFolder) break
        }
        file <- paste0(file, i)
    }
    save(..., list=objects, file=paste0(file, stamp, ".rda"))
}
