\name{add_concept}
\alias{add_concept}

\title{Insert a New Concept in a taxlist Object.}
\description{
New taxa (taxon concepts) can be inserted in an object of class
\code{\link[=taxlist-class]{taxlist}}.
}
\usage{
add_concept(taxlist, TaxonName, AuthorName, ...)
}
\arguments{
\item{taxlist}{A object of class \code{\link[=taxlist-class]{taxlist}}.}
\item{TaxonName}{A character vector with the new names.}
\item{AuthorName}{A character vector with the names of the respective authors.}
\item{...}{Additional named vectors to be included in the slot 
	\code{'taxonNames'}.}
}
\details{
Concept and usage IDs will be generated automatically.
Additional named vector will be included in the slot \code{taxonName}, when
those variables already exist in \code{taxlist}.
}
\value{
An object of class \code{\link[=taxlist-class]{taxlist}} with added names.
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\examples{
library(taxlist)
data(conosur_splist)

## Subset with Weinmannia species
Weinmannia <- subset(conosur_splist, Traits=genus == "Weinmannia")
summary(Weinmannia)

## Adding two new concepts
Weinmannia <- add_concept(Weinmannia, TaxonName=c("tax1","tax2"),
	AuthorName=c("aut1","aut2"), Link=c("link1","link2"))

## A summary again  
summary(Weinmannia)
summary(Weinmannia, "all")
}
