\name{clean}
\docType{methods}
\alias{clean}
\alias{clean,taxlist-method}

\title{Clean Orphaned Records}
\description{
Manipulation of slots may generate orphaned entries in
\code{\linkS4class{taxlist}} objects.
The function \code{clean()} deletes such entries and restores the consistency
of the objects.
}
\usage{
\S4method{clean}{taxlist}(object, ...)
}
\arguments{
\item{object}{A \code{\linkS4class{taxlist}} object.}
\item{...}{Further arguments passed from or to other methods.}
}
\details{
Cleaning of objects will follow the deletion of:
\itemize{
\item orphaned names.
\item non cited references.
\item orphaned taxon trait entries.
\item orphaned parent entries.
}
}
\value{
A clean \code{\linkS4class{taxlist}} object.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\examples{
\donttest{
library(taxlist)
data(Easplist)

## Direct manipulation of slot taxonRelations generates an invalid object
Easplist@taxonRelations <- Easplist@taxonRelations[1:5,]
summary(Easplist)

## Now apply cleaning
Easplist <- clean(Easplist)
summary(Easplist)
}
}
\keyword{methods}
