\name{add_level}
\docType{methods}
\alias{add_level}
\alias{add_level,taxlist-method}
\alias{add_level<-}
\alias{add_level<-,taxlist-method}

\title{Add or modify level entries in single concepts.}
\description{
Changes in concept identity can also implicate changes in their taxonomic
hierarchy.
}
\usage{
\S4method{add_level}{taxlist}(taxlist, ConceptID, Level, ...)

add_level(taxlist, ConceptID) <- value
}
\arguments{
\item{taxlist}{a \code{\link[=taxlist-class]{taxlist}} object.}
\item{ConceptID}{concept IDs where to add level information.}
\item{Level,value}{new value of level for the respective concepts.}
\item{...}{further arguments to pass from or to other methods.}
}
\details{
Previous to a modification in the taxonomic level of a concept, the occurrence
of the new value will be checked among the levels that are set for the
\code{\link[=taxlist-class]{taxlist}} object.
If the level is not previously set for the object, an error message will be
retrieved.

A single value for new level can be provided for multiple concepts.

It is strongly recommended to check validiy of the resulting
\code{\link[=taxlist-class]{taxlist}} object after any modification.
}
\value{
An object of class \code{\link[=taxlist-class]{taxlist}} with modified level
entries.
}
\seealso{
\code{\link{levels}}.
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
summary(Ebenaceae, 50475)

## Euclea racemosa ssp. schimperi will be elevated to species level
add_level(Ebenaceae, 50475) <- "species"

summary(Ebenaceae)
summary(Ebenaceae, 50475)
}
\keyword{methods}
