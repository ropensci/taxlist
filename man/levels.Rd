\name{levels}
\docType{methods}
\alias{levels}
\alias{levels,factor-method}
\alias{levels,taxlist-method}
\alias{levels<-}
\alias{levels<-,taxlist,character-method}

\title{Set hierarchical levels in \sQuote{taxlist} objects}
\description{
Names of levels and its hierarchical upward sequence.
}
\usage{
\S4method{levels}{factor}(x, ...)

\S4method{levels}{taxlist}(x, ...)

levels(x) <- value
}
\arguments{
\item{x}{Either a factor or a \code{\link[=taxlist-class]{taxlist}} object.}
\item{value}{Character vector with the name and the upwards sequence of levels.}
\item{...}{Additional arguments passed from or to other methods.}
}
\details{
Taxonomic levels will be handled as factors in the taxlist object.
}
\value{
A \code{\linkS4class{taxlist}} object with a subset including requested
concepts with children or parents.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(taxlist)
data(Easplist)
summary(Easplist)

## Get levels of species list
levels(Easplist)

## Add aggregate as new taxonomic level
levels(Easplist) <- c("form","variety","subspecies","species","aggregate",
	"genus","family")
summary(Easplist)
}
\keyword{methods}
