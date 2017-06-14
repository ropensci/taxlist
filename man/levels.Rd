\name{levels}
\docType{methods}
\alias{levels}
\alias{levels,factor-method}
\alias{levels,taxlist-method}
\alias{levels<-}
\alias{levels<-,taxlist-method}

\title{Set hierarchical levels in \sQuote{taxlist} objects}
\description{
Taxonomic hierarchies are set as levels in \code{\linkS4class{taxlist}} objects,
ordered from lower to higher levels.
}
\usage{
\S4method{levels}{factor}(x, ...)

\S4method{levels}{taxlist}(x, ...)
}
\arguments{
\item{x}{Either a factor or a \code{\linkS4class{taxlist}} object.}
\item{...}{Additional arguments passed from or to other methods.}
}
\details{
Taxonomic levels will be handled as factors in the \code{\linkS4class{taxlist}}
objects.
Those levels are useful for creating subsets of related groups (e.g. by
functions \code{\link{get_children}} or \code{\link{get_parents}}).
Levels in combination to parent-child relationships will be used for testing
consistency on the information of taxonomic lists.

Since this function is set as generic and masks the original
\code{\link[base]{levels}} a method for \code{'factor'} vectors is provided, as
well.
}
\value{
A \code{'character'} vector or a \code{\linkS4class{taxlist}} object with
added or modified taxonomic levels.
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
