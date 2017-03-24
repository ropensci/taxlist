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
\donttest{
library(taxlist)

## Loading GermanSL 1.3 (http://www.botanik.uni-greifswald.de/2305.html)
GermanSL <- tv2taxlist("GermanSL_1.3", file.path(path.package("taxlist"),
	"tv_data"))
summary(GermanSL, units="Mb")

## Get first 5 columns of traits
head(taxon_traits(GermanSL)[,c(1:5)])
}
}
\keyword{methods}
