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
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\seealso{
\code{\link{add_view}}, \code{\link{taxon_views}}.
}
\examples{
library(taxlist)
data(Easplist)

## Subset with Ebenaceae species
Ebenaceae <- subset(Easplist, Traits=FAMILY == "Ebenaceae")
summary(Ebenaceae)

## Adding two new concepts
Ebenaceae <- add_concept(Ebenaceae, View=1, TaxonName="Euclea acutifolia",
AuthorName="E. Mey. ex A. DC.")

## A summary again  
summary(Ebenaceae)
summary(Ebenaceae, "all")
}
