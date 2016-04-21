\name{taxonTraits}
\docType{methods}
\alias{taxonTraits}
\alias{taxonTraits,taxlist-method}
\alias{taxonTraits<-}
\alias{taxonTraits<-,taxlist,data.frame-method}
\alias{[,taxlist-method}
\alias{[<-}
\alias{[<-,taxlist-method}
\alias{$,taxlist-method}
\alias{$<-}
\alias{$<-,taxlist-method}

\title{Retrieve or Assign Slot \dQuote{taxonTraits} to a \dQuote{taxlist}
Object}
\usage{
\S4method{taxonTraits}{taxlist}(taxlist, ...)
\S4method{$}{taxlist}(x, name)
\S4method{[}{taxlist}(x, i, j, ..., drop=FALSE)

taxonTraits(taxlist) <- value
x$i <- value
x[i,j] <- value
}
\arguments{
\item{taxlist, x}{Object of class \code{taxlist}.}
\item{value}{Data frame or vector with replacement values (depending on the
	method).}
\item{...}{Further arguments passed to or from other methods.}
\item{name}{A name to access.}
\item{i,j}{Indices for access.}
\item{drop}{A logical value passed to \code{\link{Extract}}.}
}
\description{
The function \code{taxonTraits} retrieves or assigns the slot \code{taxonTraits}
to objects of the class \code{\link[=taxlist-class]{taxlist}}.
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\examples{
## Add an example
}
