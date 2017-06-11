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

\title{Extract or replace parts of a \sQuote{taxlist} object.}
\description{
Quick access to slots \code{'taxonTraits'} and \code{'taxonRelations'} in
a \code{\linkS4class{taxlist}} objects.
}
\usage{
\S4method{$}{taxlist}(x, name)

\S4method{[}{taxlist}(x, i, j, ..., slot="relations", drop=FALSE)

x$name <- value

x[i,j] <- value
}
\arguments{
\item{x}{Object of class \code{\linkS4class{taxlist}}.}
\item{slot}{To which slot will be applied the extraction or replacement.}
\item{value}{Data frame or vector with replacement values (depending on the
	method).}
\item{...}{Further arguments passed to or from other methods.}
\item{name}{A name to access.}
\item{i,j}{Indices for access.}
\item{drop}{A logical value passed to \code{\link[base]{Extract}}.}
}
\details{
While the method \code{'$'} automatically recognizes the slot queried, provided
that there is no shared column names, in the method \code{'['} you may specify
it by the argument \code{'slot'} (default for \code{'taxonRelations'}).
In the later argument you can either use the full name of slots or an alias.

In the method \code{'['}, the first index is referred to the rows in slot
\code{'taxonRelations'}, while the second index indicate the columns in either
\code{'taxonRelations'} or \code{'taxonTraits'}.
}
\value{
The method \code{'$'} retrieves a vector, while \code{'['} retrieves a subset
of input \code{'taxlist'} as a \code{\linkS4class{taxlist}} object.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\linkS4class{taxlist}}, \code{\link[taxlist]{subset}}.}
\examples{
\donttest{
library(taxlist)
library(thisdataismine)

## Loaded GermanSL 1.3 (http://www.botanik.uni-greifswald.de/2305.html)
GermanSL <- tv2taxlist("GermanSL_1.3", file.path(path.package("thisdataismine"),
	"tv_data"))

## Statistics on life forms
summary(as.factor(GermanSL$LF_LEB))

## First five concepts in this list
summary(GermanSL[1:10,], "all")
}
}
\keyword{methods}
