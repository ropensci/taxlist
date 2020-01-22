\name{taxlist-class}
\docType{class}
\alias{taxlist-class}
\alias{taxlist}

\title{Class taxlist}
\description{
Class for species (taxon) lists including synonyms, hierarchical ranks,
parent-child relationships, taxon views and taxon traits.
}
\section{Slots}{
Objects of this class are composed of three slots, every one corresponding to
a data frame:
\describe{
\item{taxonNames}{Table of taxon usage names (accepted names and synonyms).}
\item{taxonRelations}{Relations between concepts, accepted names, basionyms,
	parents and hierarchical level.}
\item{taxonTraits}{Table of taxon traits.}
\item{taxonViews}{References used to determine the respective concept
	circumscription.}
}
An additional property of \code{taxlist} objects are the taxonomic ranks, which
can be set and retrieved using the function
\code{\link[taxlist]{levels}}.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\references{
\bold{Alvarez M, Luebert F (2018).} The taxlist package: managing plant
taxonomic lists in R. \emph{Biodiversity Data Journal} 6: e23635.
\url{https://doi.org/10.3897/bdj.6.e23635}
}
\examples{
library(taxlist)

showClass("taxlist")

## Create an empty object
Splist <- new("taxlist")
}
\keyword{classes}
