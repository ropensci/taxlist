\name{backup_object}
\alias{backup_object}

\title{Backups R objects.}
\description{
Make security copies of R objects during processing its information.
This function is a wrapper for \code{\link{save}}.
}
\usage{
backup_object(..., objects, file, stamp=TRUE, overwrite=FALSE)
}
\arguments{
\item{...}{Names of the objects to be saved (either symbols or character
	strings).}
\item{objects}{A character vector indicating the names of objects to be included
	in the backup file.}
\item{file}{A character value indicating the name of the backup (rda) file.}
\item{stamp}{A logical value indicating whether time should be stamped in the
	backup name or not.}
\item{overwrite}{A logical value indicating whether existing files must be
	overwritten or not.}
}
\details{
The \code{backup_object} function create a rda file in the working directory
using \code{\link{save}}.
The argument \code{'file'} should include the name of the backup file without
extension.

For \code{'overwrite=FALSE'} (the default) a numeric suffix will be
automatically added to the file names, when previous backups sharing the same
name already exist.

Saved backups can be imported to the session by using \code{\link{load}}.
}
\value{
A rda file in the working directory.
}
\seealso{
\code{\link{save}}, \code{\link{load}}.
}
\examples{
\dontrun{
library(taxlist)
data(Easplist)

## A subset with Pseudognaphalium and relatives
Pseudognaphalium <- subset(Easplist, grepl("Pseudognaphalium", TaxonName),
	slot="names")
Pseudognaphalium <- get_parents(Easplist, Pseudognaphalium)

## Create a backup with date stamp
backup_object(Pseudognaphalium, file="Pseudonaphalium")

## The same
backup_object(objects="Pseudognaphalium", file="Pseudonaphalium")
}
}
