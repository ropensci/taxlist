\name{add_name}
\alias{add_name}

\title{Insert a New Name in a taxlist Object.}
\description{
New names (taxon usages) can be inserted for existing concepts in an object of
class \code{\link[=taxlist-class]{taxlist}}.
}
\usage{
add_name(taxlist, ConceptID, TaxonName, AuthorName, ...)
}
\arguments{
\item{taxlist}{A taxlist object including the concept to assign a new name.}
\item{ConceptID}{A integer value or vector with the respective concept IDs to be
	complemented.}
\item{TaxonName}{A character value or vector with the new names.}
\item{AuthorName}{A character value or vector with the names of the respective
	authors.}
\item{...}{Additional named vectors to be included in the slot taxonNames.}
}
\value{
An object of class \code{\link[=taxlist-class]{taxlist}} with added names.
}
\details{
Usage IDs for the new names will be generated automatically by this function.
Names only for one concept can be added at time.
For \code{'ConceptID'} longer than 1, an error message will be generated.

Argument \code{'ConceptID'} should be provided as integer, but other classes
will be coerced to integer.
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\examples{
library(taxlist)
data(EAsplist)

# Adding an hypothetical synonym for Pseudognaphalium luteoalbum
summary(EAsplist, 462)
EAsplist <- add_name(EAsplist, ConceptID=462, TaxonName="Gnaphalium novum",
AuthorName="X. Incognito")

# Display it again
summary(EAsplist, 462)
}
