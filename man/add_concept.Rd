\name{add_concept}
\alias{add_concept}

\title{Insert a New Concept in a Taxonomic List.}
\description{
New taxon concepts will be added complementing objects of class
\code{\link[=taxlist-class]{taxlist}}.
}
\usage{
add_concept(taxlist, View, TaxonName, AuthorName, ...)
}
\arguments{
\item{taxlist}{A object of class \code{\link[=taxlist-class]{taxlist}}.}
\item{View}{An integer vector with the ID of the respective taxon view.}
\item{TaxonName}{A character vector with the new names.}
\item{AuthorName}{A character vector with the names of the respective authors.}
\item{...}{Additional named vectors to be included in the slot 
	\code{taxonNames}.}
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
