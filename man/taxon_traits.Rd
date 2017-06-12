\name{taxon_traits}
\docType{methods}
\alias{taxon_traits}
\alias{taxon_traits,taxlist-method}
\alias{taxon_traits<-}
\alias{taxon_traits<-,taxlist,data.frame-method}

\title{Retrieve or replace slot \sQuote{taxonTraits} in \dQuote{taxlist}
objects.}
\description{
The slot \code{'taxonTraits'} in \code{\linkS4class{taxlist}} objects contains
attributes of taxon concepts (e.g. functional traits, etc.).
}
\usage{
\S4method{taxon_traits}{taxlist}(taxlist, ...)

taxon_traits(taxlist) <- value
}
\arguments{
\item{taxlist}{A \code{\linkS4class{taxlist}} object.}
\item{value}{Data frame to be set as slot \code{'taxonTraits'}.}
\item{...}{Further arguments passed to or from other methods.}
}
\details{
The replacement method \code{'taxon_traits<-'} should be only used when
constructing \code{\linkS4class{taxlist}} objects from an empty one.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\linkS4class{taxlist}}.
}
\examples{
library(taxlist)

data(Easplist)
summary(Easplist, units="Mb")

## Get the head of slot "taxonTraits"
head(taxon_traits(Easplist))
}
\keyword{methods}
