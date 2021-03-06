% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxon_relations.R
\name{taxon_relations}
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
\usage{
taxon_relations(taxlist, ...)

\S4method{taxon_relations}{taxlist}(taxlist, ...)

taxon_relations(taxlist) <- value

\S4method{taxon_relations}{taxlist,data.frame}(taxlist) <- value

add_concept(taxlist, TaxonName, ...)

\S4method{add_concept}{taxlist,character}(taxlist, TaxonName, Level, ...)

\S4method{add_concept}{taxlist,taxlist}(taxlist, TaxonName, insert_view, ...)

update_concept(taxlist, ConceptID, ...)

\S4method{update_concept}{taxlist,numeric}(taxlist, ConceptID, ...)
}
\arguments{
\item{taxlist}{A \linkS4class{taxlist} object.}

\item{...}{Further arguments passed among methods.}

\item{value}{A \code{data.frame} object to be set as slot \code{taxonRelations}.}

\item{TaxonName}{Character vector with the accepted name for the new taxon
concepts.}

\item{Level}{Character vector indicating the level of the concept in the
list.}

\item{insert_view}{A numeric (integer) vector, indicating the views to be
inserted in \code{taxlist} or the value \code{TRUE} (see details).}

\item{ConceptID}{Concept IDs to be updated.}
}
\value{
An object of class \linkS4class{taxlist} with added names and
concepts.
}
\description{
Retrieve the content of slot \code{taxonRelations} from a
\linkS4class{taxlist} object or replace it by a new data frame.
}
\details{
The replacement method \verb{taxon_relations<-} should be only used when
constructing \linkS4class{taxlist} objects from an empty one
(prototype).

New concepts should be first added to a \linkS4class{taxlist} object
using their respective accepted names.
Synonyms can be further provided using the function \code{\link[=add_synonym]{add_synonym()}}.

Additional named vectors can be provided to be included in slot \code{taxonNames},
in the cases where those variables already exist, otherwise they will be
ignored.

It is recommended also to provide a concept view as \code{ViewID} (see
\code{\link[=taxon_views]{taxon_views()}}).
For adding a new view, use \code{\link[=add_view]{add_view()}}.
}
\examples{
## Subset for the genus Euclea and display of slot 'taxonNames'
Euclea <- subset(x=Easplist, subset=charmatch("Euclea", TaxonName),
    slot="names")
Euclea <- get_children(Easplist, Euclea)

Euclea
taxon_relations(Euclea)

## Subset with family Ebenaceae and children
Ebenaceae <- subset(Easplist, charmatch("Ebenaceae", TaxonName))
Ebenaceae <- get_children(Easplist, Ebenaceae)

Ebenaceae
summary(object=Ebenaceae, ConceptID="all", maxsum=100)

## Adding a new concept
Ebenaceae <- add_concept(taxlist=Ebenaceae, TaxonName="Euclea acutifolia",
    AuthorName="E. Mey. ex A. DC.", Level="species", Parent=55707, ViewID=1)

## A summary again  
Ebenaceae
summary(Ebenaceae, "all", maxsum=100)

## Display two Typha species
summary(Easplist, c("Typha domingensis","Typha latifolia"))

## Update a concept
summary(Easplist, "Corchorus olitorius")
Easplist <- update_concept(taxlist=Easplist, ConceptID=155,
    Level="subspecies")
summary(Easplist, "Corchorus olitorius")

}
\seealso{
\linkS4class{taxlist}
}
\author{
Miguel Alvarez \email{kamapu78@gmail.com}
}
