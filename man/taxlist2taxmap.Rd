\name{taxlist2taxmap}
\docType{methods}
\alias{taxlist2taxmap}
\alias{taxlist2taxmap,taxlist-method}
\alias{taxmap2taxlist}

\title{Conversion among taxlist and taxmap objects}
\description{
Exchange of data between the packages \code{'taxlist'} and \code{'taxa'}.
}
\usage{
\S4method{taxlist2taxmap}{taxlist}(taxlist, ...)

## Normal function for objects of class 'Taxmap'
taxmap2taxlist(taxmap, relations, traits, synonyms, views, reindex=FALSE)
}
\arguments{
\item{taxlist,taxmap}{Input object of class \code{\linkS4class{taxlist}} or
	\code{'Taxmap'}.}
\item{relations,traits,synonyms,views}{Character values indicating the names of
	data frames in the \code{'Taxmap'} object at \code{'data'}, which should be
	used for the slots taxonRelations, taxonTraits, taxonNames, and taxonViews,
	respectively.}
\item{reindex}{Logical value indicating whether taxon IDs should be assigned
	anew or not.}
\item{...}{Additional arguments passed among methods.}
}
\details{
This function should be used for the exchange of data between the packages
\code{\link[=taxlist-package]{taxlist}} and \code{'taxa'}.
}
\value{
Depending on the applied function, either a \code{\linkS4class{taxlist}} or a
\code{'Taxmap'} object.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}) and Zachary Foster
(\email{zacharyfoster1989@gmail.com}).
}
\examples{
library(taxlist)
library(taxa)

## Example will be added
}
\keyword{methods}
