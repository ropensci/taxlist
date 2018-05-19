\name{add_synonym}
\docType{methods}
\alias{add_synonym}
\alias{add_synonym,taxlist-method}

\title{Insert synonyms in a \sQuote{taxlist} object.}
\description{
New names (taxon usages) assigned as synonyms of existing taxon concepts can be
added to existing concepts in \code{\linkS4class{taxlist}} object.
}
\usage{
\S4method{add_synonym}{taxlist}(taxlist, ConceptID, TaxonName, AuthorName, ...)
}
\arguments{
\item{taxlist}{A \code{\linkS4class{taxlist}} object.}
\item{ConceptID}{Concept IDs where to add new synonyms.}
\item{TaxonName}{New taxon names (synonyms) as character values.}
\item{AuthorName}{Names of the respective authors.}
\item{...}{Additional named vectors to be included in the slot
	\code{'taxonNames'}.}
}
\details{
Multiple synonyms can be provided for a single concept or for multiple concepts
at once.
In the later case the vector \code{'ConceptID'} have to be of the same length as
all further input vectors.
}
\value{
A \code{\linkS4class{taxlist}} object with added synonyms.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(taxlist)
data(Easplist)

## Display Diospyros scabra
summary(Easplist, "Diospyros scabra")

## Add Maba scabra as synonym
Easplist <- add_synonym(Easplist, 51793, TaxonName="Maba scabra",
	AuthorName="Chiov.")
summary(Easplist, "Diospyros scabra")
}
\keyword{methods}
