\name{add_parent}
\docType{methods}
\alias{add_parent}
\alias{add_parent,taxlist-method}
\alias{add_parent<-}
\alias{add_parent<-,taxlist-method}

\title{Add or modify parent entries in taxon concepts.}
\description{
Add information of parent for determined concepts in a
\code{\linkS4class{taxlist}} object.
}
\usage{
\S4method{add_parent}{taxlist}(taxlist, ConceptID, Parent, ...)

add_parent(taxlist, ConceptID) <- value
}
\arguments{
\item{taxlist}{A \code{\linkS4class{taxlist}} object.}
\item{ConceptID}{Concept IDs where to add level information.}
\item{Parent,value}{Concept IDs of the respective parent taxa.}
\item{...}{Further arguments to pass from or to other methods.}
}
\details{
Previous to add a parent information, this function will check the occurrence of
the assigned concept within the input \code{\linkS4class{taxlist}} object.
If the new parent value is not occurring, this function retrieves an error
message.

A single value for new parent entry can be provided for multiple concepts.

When the input \code{\linkS4class{taxlist}} object also contains information on
taxonomic levels, it is strongly recommended to check validity of the resulting
object (see \code{\link{validObject}}).
}
\value{
An object of class \code{\linkS4class{taxlist}} with modified parent entries.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link{validObject}}, \code{\link[taxlist]{levels}},
\code{\link{taxon_relations}}.
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
