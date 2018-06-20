\name{levels}
\docType{methods}
\alias{levels}
\alias{levels,factor-method}
\alias{levels,taxlist-method}
\alias{levels<-}
\alias{levels<-,taxlist-method}

\title{Set and Retrieves Hierarchical Levels}
\description{
Taxonomic hierarchies can be set as levels in \code{\linkS4class{taxlist}}
objects, ordered from lower to higher levels.

Add taxonomic levels for specific taxon concepts in a
\code{\linkS4class{taxlist}} object.
Also changes in concept circumscription may implicate changes in its taxonomic
hierarchy.
}
\usage{
\S4method{levels}{factor}(x, ...)

\S4method{levels}{taxlist}(x, ...)
}
\arguments{
\item{x}{A \code{\linkS4class{taxlist}} object.}
\item{...}{Additional arguments passed among methods.}
}
\details{
Taxonomic levels will be handled as factors in the \code{\linkS4class{taxlist}}
objects.
Those levels are useful for creating subsets of related groups (e.g. by
functions \code{\link[=get_children]{get_children()}} or
\code{\link[=get_parents]{get_parents()}}).
Levels in combination to parent-child relationships will be further used for
checking consistency of taxonomic lists.

A replacement method of the form \code{'levels(x) <- value'} it is also
implemented.
}
\value{
A \code{'character'} vector or a \code{\linkS4class{taxlist}} object with
added or modified taxonomic levels.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\examples{
library(taxlist)
data(Easplist)
summary(Easplist)

## Get levels of species list
levels(Easplist)

## Add aggregate as new taxonomic level
levels(Easplist) <- c("form","variety","subspecies","species","complex",
	"aggregate","genus","family")
summary(Easplist)
}
\keyword{methods}
