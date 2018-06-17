\name{taxon_relations}
\docType{methods}
\alias{taxon_relations}
\alias{taxon_relations,taxlist-method}
\alias{taxon_relations<-}
\alias{taxon_relations<-,taxlist,data.frame-method}
\alias{add_concept}
\alias{add_concept,taxlist,character-method}
\alias{add_concept,taxlist,taxlist-method}
\alias{update_concept}
\alias{update_concept,taxlist,numeric-method}

\title{Retrieve or replace slot taxonRelations in taxlist objects}
\description{
Retrieve the content of slot \code{'taxonRelations'} from a
\code{\linkS4class{taxlist}} object or replace it by a new data frame.
}
\usage{
\S4method{taxon_relations}{taxlist}(taxlist, ...)

taxon_relations(taxlist) <- value

\S4method{add_concept}{taxlist,character}(taxlist, TaxonName, Level, ...)

\S4method{add_concept}{taxlist,taxlist}(taxlist, TaxonName, insert_view, ...)

\S4method{update_concept}{taxlist,numeric}(taxlist, ConceptID, ...)
}
\arguments{
\item{taxlist}{A \code{\linkS4class{taxlist}} object.}
\item{value}{A \code{'data.frame'} object to be set as slot
	\code{'taxonRelations'}.}
\item{TaxonName}{Character vector with the accepted name for the new taxon
	concepts.}
\item{Level}{Character vector indicating the level of the concept in the list.}
\item{insert_view}{A numeric (integer) vectors, indicating the views of
	\code{'TaxonName'} to be inserted in \code{'taxlist'} or the value
	\code{'TRUE'} (see details).}
\item{ConceptID}{Concept IDs to be updated.}
\item{...}{Further arguments passed among methods.}
}
\details{
The replacement method \code{'taxon_relatiions<-'} should be only used when
constructing \code{\linkS4class{taxlist}} objects from an empty one (prototype).

New concepts should be first added to a \code{\linkS4class{taxlist}} object
using their respective accepted names.
Synonyms can be further provided using the function \code{\link{add_synonym}}.

Additional named vectors can be provided to be included in slot
\code{'taxonNames'}, in the cases where those variables already exist, otherwise
they will be ignored.

It is recommended also to provide a concept view as \code{'ViewID'} (see
\code{\link{taxon_views}}).
For adding a new view, use \code{\link{add_view}}.
}
\value{
An object of class \code{\linkS4class{taxlist}} with added names and concepts.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\seealso{
\code{\linkS4class{taxlist}}, \code{\link{add_synonym}}.
}
\examples{
library(taxlist)
data(Easplist)

## Subset for the genus Euclea and display of slot 'taxonNames'
Euclea <- subset(Easplist, charmatch("Euclea", TaxonName), slot="names")
Euclea <- get_children(Easplist, Euclea)

summary(Euclea)
taxon_relations(Euclea)

## Subset with family Ebenaceae and children
Ebenaceae <- subset(Easplist, charmatch("Ebenaceae", TaxonName))
Ebenaceae <- get_children(Easplist, Ebenaceae)

summary(Ebenaceae)
summary(Ebenaceae, "all", maxsum=100)

## Adding a new concept
Ebenaceae <- add_concept(Ebenaceae, TaxonName="Euclea acutifolia",
AuthorName="E. Mey. ex A. DC.", Level="species", Parent=55707, ViewID=1)

## A summary again  
summary(Ebenaceae)
summary(Ebenaceae, "all", maxsum=100)

## Display two Typha species
summary(Easplist, c("Typha domingensis","Typha latifolia"))

## Update a concept
summary(Easplist, "Corchorus olitorius")
Easplist <- update_concept(Easplist, 155, Level="subspecies")
summary(Easplist, "Corchorus olitorius")
}
\keyword{methods}
