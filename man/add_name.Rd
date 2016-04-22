\name{add_name}
\alias{add_name}

\title{Insert a New Name in a \sQuote{taxlist} Object.}
\description{
New names (taxon usages) can be inserted for existing concepts in an object of
class \code{\link[=taxlist-class]{taxlist}}.
}
\usage{
add_name(taxlist, ConceptID, TaxonName, AuthorName, ...)
}
\arguments{
\item{taxlist}{A \code{\link[=taxlist-class]{taxlist}} object including the
	concept to assign a new name.}
\item{ConceptID}{An integer vector with the respective concept IDs to be
	complemented.}
\item{TaxonName}{A character vector with the new names.}
\item{AuthorName}{A character vector with the names of the respective authors.}
\item{...}{Additional named vectors to be included in the slot
	\code{taxonNames}.}
}
\value{
An object of class \code{\link[=taxlist-class]{taxlist}} with added names.
}
\details{
Usage IDs for the new names will be generated automatically according to the
highest existing usage ID.
Names only for one concept can be added at time.
For \code{'ConceptID'} longer than 1, an error message will be generated.

Argument \code{'ConceptID'} should be provided as integer, but other classes
suitable to be coerced to integer will be accepted.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\examples{
library(taxlist)
data(Easplist)

# Adding an hypothetical synonym for Pseudognaphalium luteoalbum
summary(Easplist, 462)
Easplist <- add_name(Easplist, ConceptID=462, TaxonName="Gnaphalium novum",
AuthorName="X. Incognito")

# Display it again
summary(Easplist, 462)
}
