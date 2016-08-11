\name{merge_taxa}
\docType{methods}
\alias{merge_taxa}
\alias{merge_taxa,taxlist,numeric-method}

\title{Merge Concepts and split concepts.}
\description{
Merge concepts to species or genus level.
}
\usage{
\S4method{merge_taxa}{taxlist,integer}(object, ConceptID, AcceptedName,
	overwrite_old=FALSE)
}
\arguments{
\item{object}{Object of class \code{taxlist}.}
\item{ConceptID}{Integer vector with IDs of concepts to be merged.}
\item{AcceptedName}{ID of the name to be used as accepted name for the new
	concept.}
\item{overwrite_old}{Logical value indicating if column
	\code{".TaxonConceptID_old"} should be overwritten or not.}
}
\details{
All Concepts will be included in one
}
\value{
An object of class \code{\link[=taxlist-class]{taxlist}} with merged concepts.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\examples{
library(taxlist)
data(Easplist)

## Subset with Cyperus species
Cyperus <- subset(Easplist, Traits=GENUS == "Cyperus")
summary(Cyperus)

## Merge to species level
Cyperus <- merge_taxa(Cyperus)
summary(Cyperus)

## Merge Cyperaceae to genus level
Cyperus <- subset(Easplist, Traits=FAMILY == "Cyperaceae")
summary(Cyperus)

## Merge to species level
Cyperus <- merge_taxa(Cyperus, level="genus")
summary(Cyperus)

## Display 3 first genera
Cyperus$genus_name[1:3]
summary(Cyperus[1:3,], "all")
}
\keyword{methods}
