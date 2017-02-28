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
Quick access to slots \sQuote{taxonTraits} and \sQuote{taxonRelations} in
a \code{\linkS4class{taxlist} object} object.
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
\item{value}{data frame or vector with replacement values (depending on the
	method).}
\item{...}{further arguments passed to or from other methods.}
\item{name}{a name to access.}
\item{i,j}{indices for access.}
\item{drop}{a logical value passed to \code{\link[base]{Extract}}.}
}
\details{
These methods provide a quick way to retrieve or replace information contained
in the slots \code{taxonRelations} and \code{taxonTraits}.
While the method \code{$} automatically recognizes the slot queried, in the
method \code{[} you may specify it by the argument \code{slot} (default for
\code{taxonRelations}).

Argument \code{slot} contains either the textual name of the slot or an alias.
}
\value{
Either a vector or a subset of the input \code{\linkS4class{taxlist} object}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\linkS4class{taxlist}}.}
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
