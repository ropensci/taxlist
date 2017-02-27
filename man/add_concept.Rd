\name{add_concept}
\docType{methods}
\alias{add_concept}
\alias{add_concept,taxlist-method}

\title{Insert a New Concept in a Taxonomic List.}
\description{
New taxon concepts will be added complementing objects of class
\code{\link[=taxlist-class]{taxlist}}.
}
\usage{
\S4method{add_concept}{taxlist}(taxlist, TaxonName, ...)
}
\arguments{
\item{taxlist}{An object of class \code{\link[=taxlist-class]{taxlist}}.}
\item{TaxonName}{Character vector with the accepted name for the new taxon
	concepts.}
\item{...}{Additional named vectors to be included in the slots 
	\code{taxonNames} or \code{taxonRelations}.}
}
\details{
Concept and usage IDs will be generated automatically according to the highest
existing IDs.
Additional named vectors will be included in the slot \code{taxonNames}, in the
cases where those variables already exist, otherwise they will be ignored.

Be aware that you may set the concept view (see \code{\link{taxon_views}}) or
even insert new views (\code{\link{add_view}}) previous to add a concept.
}
\value{
An object of class \code{\link[=taxlist-class]{taxlist}} with added names and
concepts.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link{add_view}}, \code{\link{taxon_views}}.
}
\examples{
library(taxlist)
data(Easplist)

## Subset with family Ebenaceae and children (see also get_children)
Ebenaceae <- subset(Easplist, charmatch("Ebenaceae", TaxonName))
Ebenaceae <- get_children(Easplist, Ebenaceae)

summary(Ebenaceae)
summary(Ebenaceae, "all", maxsum=100)

## Adding a new concept
Ebenaceae <- add_concept(Ebenaceae, TaxonName="Euclea acutifolia",
AuthorName="E. Mey. ex A. DC.", Level="species", Parent=53689, View=1)

## A summary again  
summary(Ebenaceae)
summary(Ebenaceae, "all", maxsum=100)
}
\keyword{methods}
