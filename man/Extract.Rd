\name{Extract}
\docType{methods}
\alias{Extract}
\alias{[}
\alias{[,taxlist-method}
\alias{[<-}
\alias{[<-.taxlist}
\alias{[<-,taxlist-method}
\alias{$,taxlist-method}
\alias{$<-}
\alias{$<-.taxlist}
\alias{$<-,taxlist-method}

\title{Extract or replace parts of a \sQuote{taxlist} object.}
\description{
Quick access to slots \code{'taxonTraits'} and \code{'taxonRelations'} in
a \code{\linkS4class{taxlist}} objects.
}
\usage{
\S4method{$}{taxlist}(x, name)

\S4method{[}{taxlist}(x, i, j, ..., slot="relations", drop=FALSE)

\method{$}{taxlist}(x, name, value) <- value

\method{[}{taxlist}(x, i, j, slot="relations", value) <- value
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
library(taxlist)

data(Easplist)

## Statistics on life forms
summary(as.factor(Easplist$LifeForm))

## First ten concepts in this list
summary(Easplist[1:10,], "all")
}
\keyword{methods}
