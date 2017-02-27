\name{add_synonym}
\docType{methods}
\alias{add_synonym}
\alias{add_synonym,taxlist-method}

\title{Insert Synonyms in a Taxonomic List.}
\description{
New names (taxon usages) assigned as synonyms of existing taxon concepts can be
inserted for existing concepts in an object of
class \code{\link[=taxlist-class]{taxlist}}.
}
\usage{
\S4method{add_synonym}{taxlist}(taxlist, ConceptID, TaxonName, AuthorName, ...)
}
\arguments{
\item{taxlist}{A \code{\link[=taxlist-class]{taxlist}} object including the
	concept to assign a new name.}
\item{ConceptID}{Integer vector with the respective concept IDs to be
	complemented.}
\item{TaxonName}{Character vector with the new names (synonyms).}
\item{AuthorName}{Character vector with the names of the respective authors.}
\item{...}{Additional named vectors to be included in the slot
	\code{taxonNames}.}
}
\value{
An object of class \code{\link[=taxlist-class]{taxlist}} with added names.
}
\details{
Detail later
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
## Add an example
}
