\name{add_parent}
\docType{methods}
\alias{add_parent}
\alias{add_parent,taxlist-method}
\alias{add_parent<-}
\alias{add_parent<-,taxlist-method}

\title{Add or modify parent entries in single concepts.}
\description{
Add information of parent for single or multiple concepts in a
\code{\link[=taxlist-class]{taxlist}} object.
}
\usage{
\S4method{add_parent}{taxlist}(taxlist, ConceptID, Parent, ...)

add_parent(taxlist, ConceptID) <- value
}
\arguments{
\item{taxlist}{a \code{\link[=taxlist-class]{taxlist}} object.}
\item{ConceptID}{concept IDs where to add level information.}
\item{Parent,value}{new value of level for the respective concepts.}
\item{...}{further arguments to pass from or to other methods.}
}
\details{
The occurrence of the new parent value among the concept IDs will be checked by
the function.
If the new parent value is not occurring as concept ID, the function retrieves
an error message.

A single value for new parent entry can be provided for multiple concepts.

It is strongly recommended to check validiy of the resulting
\code{\link[=taxlist-class]{taxlist}} object after any modification.
}
\value{
An object of class \code{\link[=taxlist-class]{taxlist}} with modified entries.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(taxlist)
data(Easplist)

## Subset with family Ebenaceae and children (see also get_children)
Ebenaceae <- subset(Easplist, charmatch("Ebenaceae", TaxonName))
Ebenaceae <- get_children(Easplist, Ebenaceae)

## Add a new level in the resulting object
levels(Ebenaceae) <- c(levels(Ebenaceae), "order")

## Add the order Ericales as new concept
Ebenaceae <- add_concept(Ebenaceae, TaxonName="Ericales", Level="order")
summary(Ebenaceae, "all", maxsum=100)

## Now set the order Ericales as parent of Ebenaceae
taxon_relations(Ebenaceae)
add_parent(Ebenaceae, 54636) <- 54637
taxon_relations(Ebenaceae)
}
\keyword{methods}
