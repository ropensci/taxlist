\name{tax2traits}
\docType{methods}
\alias{tax2traits}
\alias{tax2traits,taxlist-method}

\title{Add taxonomic information to slot 'taxonTraits'.}
\description{
Taxonomic classification can be included in \code{\linkS4class{taxlist}}
objects.
Nevertheless for some analyses it may be more convenient to insert such
information in the slot \code{'taxonTraits'}.
}
\usage{
\S4method{tax2traits}{taxlist}(object, get_names=FALSE, ...)
}
\arguments{
\item{object}{An object of class \code{\linkS4class{taxlist}}.}
\item{get_names}{Logical value indicating whether taxon names should be
	retrieved instead of taxon IDs}
\item{...}{Further arguments to be passed among methods.}
}
\details{
This function can only be applied to objects containing parent-child
relationships and information on taxonomic levels.
}
\value{
An object of class \code{\linkS4class{taxlist}} with taxonomy added as traits.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link{add_trait}}.
}
\examples{
library(taxlist)
data(Easplist)

## Family Acanthaceae with children
Easplist <- subset(Easplist, TaxonName == "Acanthaceae", slot="names",
	keep_children=TRUE)
summary(Easplist)

## Insert taxonomy to taxon traits
Easplist <- tax2traits(Easplist, get_names=TRUE)
head(taxon_traits(Easplist))
}
