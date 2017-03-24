\name{get_children}
\docType{methods}
\alias{get_children}
\alias{get_children,taxlist,numeric-method}
\alias{get_children,taxlist,taxlist-method}
\alias{get_parents}
\alias{get_parents,taxlist,numeric-method}
\alias{get_parents,taxlist,taxlist-method}

\title{Retrieve children or parents of determined concepts.}
\description{
Retrieve all children or all parents of a queried taxon concept.
}
\usage{
\S4method{get_children}{taxlist,numeric}(taxlist, ConceptID, ...)

\S4method{get_children}{taxlist,taxlist}(taxlist, ConceptID, ...)

\S4method{get_parents}{taxlist,numeric}(taxlist, ConceptID, ...)

\S4method{get_parents}{taxlist,taxlist}(taxlist, ConceptID, ...)
}
\arguments{
\item{taxlist}{A \code{\linkS4class{taxlist}} object.}
\item{ConceptID}{Concept IDs for selecting parents or children or a subset of
	\sQuote{'taxlist'}.}
\item{...}{Further arguments to pass from or to other methods.}
}
\details{
This function produces subsets of \code{\linkS4class{taxlist}} objects including
all children or parents of queried taxon concepts.
Multiple concepts can be queried in this function.
The argument \code{'ConceptID'} can be a vector of concept IDs or a subset of the
input \code{'taxlist'} object.
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

## Subset with family Ebenaceae and children
Ebenaceae <- subset(Easplist, charmatch("Ebenaceae", TaxonName))
Ebenaceae <- get_children(Easplist, Ebenaceae)

summary(Ebenaceae)
summary(Ebenaceae, "all", maxsum=100)

## Get parents of Diospyros tricolor
Diostri <- subset(Easplist, TaxonConceptID == 52403, slot="relations")
Diostri <- get_parents(Easplist, Diostri)

summary(Diostri)
summary(Diostri, "all")
}
\keyword{methods}
