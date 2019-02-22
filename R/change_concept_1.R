\name{backup_object}
\alias{backup_object}
\alias{load_last}

\title{Make and load Backups of R Objects}
\description{
When work with data becomes risky, the best practice is to produce backup files.
The function of \code{'backup_object'} is a wrapper of
\code{\link{save}}, adding a time stamp and a suffix to the name of the
resulting file (an R image file with extension \bold{*.rda}).
The function \code{'load_last'} is adapted to this style, loading the newest
version to the session.
}
\usage{
backup_object(..., objects, file, stamp=TRUE, overwrite=FALSE)

load_last(file)
}
\arguments{
\item{...}{Names of the objects to be saved (either symbols or character
	strings).}
\item{objects}{A character vector indicating the names of objects to be included
	in the backup file.}
\item{file}{A character value indicating the name of the backup file, without
	the extension.}
\item{st