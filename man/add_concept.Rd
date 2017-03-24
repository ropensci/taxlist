\name{add_concept}
\docType{methods}
\alias{add_concept}
\alias{add_concept,taxlist-method}

\title{Insert a new taxon concept in a \sQuote{taxlist} object.}
\description{
New taxon concepts will be added, complementing taxonomic lists formatted as
\code{\linkS4class{taxlist}} objects.
}
\usage{
\S4method{add_concept}{taxlist}(taxlist, TaxonName, Level, ...)
}
\arguments{
\item{taxlist}{An object of class \code{\linkS4class{taxlist}}.}
\item{TaxonName}{Character vector with the accepted name for the new taxon
	concepts.}
\item{Level}{Character vector indicating the level of the concept in the list.}
\item{...}{Additional named vectors to be included in the slots 
	\code{'taxonNames'} or \code{'taxonRelations'}.}
}
\details{
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
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link{add_synonym}}, \code{\link{taxon_views}}, \code{\link{add_view}}.
}
\examples{
library(taxlist)
data(Easplist)

## Subset with family Ebenaceae and children
Ebenaceae <- subset(Easplist, charmatch("Ebenaceae", TaxonName))
Ebenaceae <- get_children(Easplist, Ebenaceae)

summary(Ebenaceae)
summary(Ebenaceae, "all", maxsum=100)

## Adding a new concept
Ebenaceae <- add_concept(Ebenaceae, TaxonName="Euclea acutifolia",
AuthorName="E. Mey. ex A. DC.", Level="species", Parent=53689, ViewID=1)

## A summary again  
summary(Ebenaceae)
summary(Ebenaceae, "all", maxsum=100)
}
\keyword{methods}
