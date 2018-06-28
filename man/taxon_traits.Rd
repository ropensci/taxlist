\name{taxon_traits}
\docType{methods}
\alias{taxon_traits}
\alias{taxon_traits,taxlist-method}
\alias{taxon_traits<-}
\alias{taxon_traits<-,taxlist,data.frame-method}
\alias{update_trait}
\alias{update_trait,taxlist,numeric-method}

\title{Manipulation of taxon traits in taxlist objects.}
\description{
The slot \code{'taxonTraits'} in \code{\linkS4class{taxlist}} objects contains
attributes of taxon concepts (e.g. functional traits).
These functions are suitable for replacing, retrieving and appending trait
information in taxonomic lists.
}
\usage{
\S4method{taxon_traits}{taxlist}(taxlist, ...)

taxon_traits(taxlist) <- value

\S4method{update_trait}{taxlist,numeric}(taxlist, ConceptID, ...)
}
\arguments{
\item{taxlist}{A \code{\linkS4class{taxlist}} object.}
\item{ConceptID}{A numeric vector with the respective taxon concept IDs.}
\item{value}{Data frame to be set as slot \code{'taxonTraits'}.}
\item{...}{Further arguments to be passed among methods.}
}
\details{
Taxon traits are contained in a data frame at the slot \code{'taxonTraits'} in
\code{\linkS4class{taxlist}} objects.
To optimise space, this data frame contain only entries for those concepts with
information, while taxa with no information are skipped from this table.
Thus appending new variables may also have to include new rows in this slot,
which is automatically carried out by this function.

The replacement method \code{'taxon_traits<-'} should be only used when
constructing \code{\linkS4class{taxlist}} objects from an empty one.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
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
