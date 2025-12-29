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
#'     strings) in `backup_object()`. In `load_last()`, arguments passed to
#'     `sort_backups()`.
#' @param objects A character vector indicating the names of objects to be
#'     included in the backup file.
#' @param file A character value indicating the name of the backup file, without
#'     the extension.
#' @param stamp A logical value indicating whether time should be stamped in the
#'     backup name or not.
#' @param sep A character value used to separate backup's name from stamp and
#'     from the suffix.
#' @param date_format A character value indicating the format used for the
#'     file stamp. See [strptime()].
#' @param time_format A character value indicating the format used for the
#'     the time (not including the date), which will be used for the invisible
#'     report in `backup_object()`. See [strptime()].
#' @param fext A character value indicating the file extension (including the
#'     dot symbol).
#' @param overwrite A logical value indicating whether existing files must be
#'     overwritten or not.
#' @param fext A character value indicating the file extension (including the
#'     dot symbol).
#' @param name A character value indicating the root of the backup's name.
#' @param path A character value indicating the path to the folder containing
#'     the backup files.
#' @param choice An integer value indicating the backup file to be used for
#'     recovery. This value refers to the row in the output of `sort_backups()`.
#'     If not provided, `load_last()` will select the newest backup.
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
#' @return
#' The function `backup_object()` writes an R-image with extension \bold{*.rda}
#' and an invisible vector with the name of the backup, its absolute path and
#' a time stamp.
#'
#' The function `sort_backups()` returns a data frame including the sorted names
#' of backup files from the oldest to the newest.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @seealso [save()], [load()].
#'
#' @example examples/backup_object.R
#'
#' @rdname backup_object
#'
#' @export
backup_object <- function(
  ..., objects = character(), file, stamp = TRUE,
  sep = "_", date_format = "%Y-%m-%d", time_format = "%H:%M:%S",
  overwrite = FALSE
) {
  file2 <- basename(file_path_sans_ext(file))
  path <- dirname(file)
  inFolder <- file_path_sans_ext(list.files(path = path, pattern = ".rda"))
  if (stamp) {
    file2 <- paste0(
      file2, sep,
      format(Sys.time(), format = date_format)
    )
  }
  if (!overwrite) {
    file2 <- id_solver(insert = file2, to = inFolder, sep = sep)
  }
  file3 <- file.path(path, paste0(file2, ".rda"))
  save(..., list = objects, file = file.path(path, paste0(file2, ".rda")))
  message(paste0("Backup saved as '", file3, "'"))
  invisible(c(
    filename = paste0(file2, ".rda"), path = file3,
    abspath = file_path_as_absolute(file3),
    timestamp = format(Sys.time(),
      format = paste(date_format, time_format)
    )
  ))
}

#' @rdname backup_object
#' @aliases sort_backups
#' @export
sort_backups <- function(
  name, path = ".", date_format = "%Y-%m-%d",
  fext = ".rda", sep = "_"
) {
  inFolder <- list.files(path = path, pattern = fext)
  inFolder <- inFolder[grepl(name, inFolder, fixed = TRUE)]
  if (!length(inFolder)) {
    stop("The requested backup is missing.")
  }
  # Extract information using patterns
  pattern <- paste0(
    "^", name, sep,
    "([0-9\\-]+)", # date
    "(?:", sep, "(\\d+))?", # optional suffix
    "\\", fext, "$"
  )
  matches <- regexec(pattern, inFolder)
  parts <- regmatches(inFolder, matches)
  time_stamp <- as.Date(sapply(parts, function(x) x[2]), format = date_format)
  suffix <- as.integer(sapply(parts, function(x) {
    ifelse(length(x) >= 3, x[3],
      NA
    )
  }))
  suffix[is.na(suffix)] <- 0
  # Output object
  OUT <- data.frame(
    date = time_stamp,
    suffix = suffix,
    filename = inFolder,
    abspath = sapply(file.path(path, inFolder), file_path_as_absolute)
  )
  OUT <- OUT[order(OUT$date, OUT$suffix), ]
  OUT$order <- c(seq_len(nrow(OUT)))
  rownames(OUT) <- NULL
  return(OUT)
}

#' @rdname backup_object
#' @aliases load_last
#' @export
load_last <- function(file, path, ..., choice) {
  if (!missing(path)) {
    file <- file.path(path, file)
  }
  path <- dirname(file)
  file <- basename(file)
  OUT <- sort_backups(name = file, path = path, ...)
  if (missing(choice)) {
    choice <- nrow(OUT)
  }
  load(OUT$abspath[choice], envir = .GlobalEnv)
  message(paste0("Loading file '", OUT$filename[choice], "' to session."))
  invisible(OUT[choice, ])
}
