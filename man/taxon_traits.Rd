\name{taxon_traits}
\docType{methods}
\alias{taxon_traits}
\alias{taxon_traits,taxlist-method}
\alias{taxon_traits<-}
\alias{taxon_traits<-,taxlist,data.frame-method}

\title{Retrieve or Assign Slot \dQuote{taxonTraits} to a \dQuote{taxlist}
Object.}
\description{
The function \code{taxon_traits} retrieves or assigns the slot
\code{taxonTraits} to objects of the class
\code{\link[=taxlist-class]{taxlist}}.
}
\usage{
\S4method{taxon_traits}{taxlist}(taxlist, ...)

taxon_traits(taxlist) <- value
}
\arguments{
\item{taxlist}{Object of class \code{\link[=taxlist-class]{taxlist}}.}
\item{value}{Data frame or vector with replacement values (depending on the
	method).}
\item{...}{Further arguments passed to or from other methods.}
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link[=taxlist-class]{taxlist}}.
}
\examples{
\donttest{
library(taxlist)

## Loading GermanSL 1.3 (http://www.botanik.uni-greifswald.de/2305.html)
GermanSL <- tvsplist("GermanSL_1.3", file.path(path.package("taxlist"),
	"tv_data"))
summary(GermanSL, units="Mb")

## Get first 5 columns of traits
head(taxon_traits(GermanSL)[,c(1:5)])
}
}
\keyword{methods}
