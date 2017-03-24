\name{clean}
\docType{methods}
\alias{clean}
\alias{clean,taxlist-method}

\title{Clean orphaned records.}
\description{
Direct deletions in slot \code{'taxonRelations'} may generate orphaned entries.
}
\usage{
\S4method{clean}{taxlist}(object, ...)
}
\arguments{
\item{object}{A \code{\linkS4class{taxlist}} object.}
\item{...}{Further arguments passed from or to other methods.}
}
\details{
This function should be applied to optimize the allocated size of a
\code{\linkS4class{taxlist}} object and to assure its validity.

The steps followed for the cleaning process are:
\itemize{
	\item Orphaned names.
	\item Non cited references.
	\item Orphaned taxon trait entries.
	\item Orphaned parent entries.
}
}
\value{
A clean \code{\linkS4class{taxlist}} object.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
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
