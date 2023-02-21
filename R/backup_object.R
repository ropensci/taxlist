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
#' @example examples/backup_object.R
#'
#' @rdname backup_object
#'
#' @export
backup_object <- function(..., objects = character(), file, stamp = TRUE,
                          overwrite = FALSE) {
  file2 <- basename(file_path_sans_ext(file))
  path <- dirname(file)
  inFolder <- file_path_sans_ext(list.files(path = path, pattern = ".rda"))
  if (stamp) file2 <- paste0(file2, "_", Sys.Date())
  if (!overwrite) {
    file2 <- id_solver(insert = file2, to = inFolder, sep = "_")
  }
  save(..., list = objects, file = file.path(path, paste0(file2, ".rda")))
  message(paste0(
    "Backup saved as '", file.path(path, paste0(file2, ".rda")),
    "'"
  ))
}

#' @rdname backup_object
#' @export
load_last <- function(file, fext = ".rda") {
  OUT <- sort_backups(file = file, fext = fext)
  message(paste0("Loading file '", OUT$filename[nrow(OUT)], "' to session."))
  path <- OUT$path[nrow(OUT)]
  if (length(path) == 1 & path[1] == ".") {
    load(OUT$filename[nrow(OUT)],
      envir = .GlobalEnv
    )
  } else {
    load(file.path(paste(path, collapse = "/"), OUT$filename[nrow(OUT)]),
      envir = .GlobalEnv
    )
  }
}
