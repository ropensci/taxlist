\name{as.phylo}
\docType{methods}
\alias{as.phylo}
\alias{as.phylo,taxlist-method}
\alias{as.multiPhylo}
\alias{as.multiPhylo,taxlist-method}

\title{Conversion to phylo object}
\description{
Plotting of taxonomic relationships between taxon concepts within
\code{\linkS4class{taxlist}} objects requires a transformation to \code{'phylo'}
class.
}
\usage{
\S4method{as.phylo}{taxlist}(x, print_name=FALSE, include_author=FALSE,
	second_mention=FALSE, ...)

\S4method{as.multiPhylo}{taxlist}(x, ...)
}
\arguments{
\item{x}{A \code{\linkS4class{taxlist}} object.}
\item{print_name}{Logical value, whether \code{\link{print_name}} should be used
	for tips and nodes labels or not.}
\item{include_author}{Logical value, whether authors' names should be included
	in the labels or not.}
\item{second_mention}{Logical value, passed to \code{\link{print_name}}.}
\item{...}{Further arguments passed among methods.}
}
\value{
An object of class \code{'phylo'} or \code{'multiPhylo'}.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\seealso{
\code{\link[ape]{as.phylo}}.
}
\examples{
\dontrun{
library(taxlist)
library(ape)

sedges <- subset(Easplist, TaxonName == "Cyperaceae", slot="names",
	keep_children=TRUE)
sedges <- subset(sedges, Level == "variety", slot="relations",
	keep_parents=TRUE)
sedges <- as.phylo(sedges)
plot(sedges)

sedges2 <- subset(Easplist, TaxonName %in% c("Kyllinga","Isolepis"), slot="names",
	keep_children=TRUE, keep_parents=TRUE)
sedges2 <- as.phylo(sedges2)
plot(sedges2)
}
}
