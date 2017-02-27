\name{Extract}
\docType{methods}
\alias{Extract}
\alias{[}
\alias{[,taxlist-method}
\alias{[<-}
\alias{[<-,taxlist-method}
\alias{$,taxlist-method}
\alias{$<-}
\alias{$<-,taxlist-method}

\title{Retrieve or Assign Slot \dQuote{taxonTraits} to a \dQuote{taxlist}
Object.}
\description{
The function \code{taxon_traits} retrieves or assigns the slot
\code{taxonTraits} to objects of the class
\code{\link[=taxlist-class]{taxlist}}.
}
\usage{
\S4method{$}{taxlist}(x, name)
\S4method{[}{taxlist}(x, i, j, ..., slot="relations", drop=FALSE)

x$name <- value
x[i,j] <- value
}
\arguments{
\item{x}{object of class \code{\link[=taxlist-class]{taxlist}}.}
\item{slot}{to which slot will be applied the extraction or replacement.}
\item{value}{Data frame or vector with replacement values (depending on the
	method).}
\item{...}{Further arguments passed to or from other methods.}
\item{name}{A name to access.}
\item{i,j}{Indices for access.}
\item{drop}{A logical value passed to \code{\link{Extract}}.}
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link[=taxlist-class]{taxlist}}.
}
\examples{
library(taxlist)

## Loading GermanSL 1.3 (http://www.botanik.uni-greifswald.de/2305.html)
GermanSL <- tvsplist("GermanSL_1.3", file.path(path.package("taxlist"),
	"tv_data"))

## Statistics on life forms
summary(as.factor(GermanSL$LF_LEB))

## First five concepts in this list
summary(GermanSL[1:10,], "all")
}
\keyword{methods}
