\name{summary}
\docType{methods}
\alias{summary}
\alias{summary,taxlist-method}

\title{Print Overview for taxlist Objects.}
\description{
A method to display either an overview of the content of
\code{\link[=taxlist-class]{taxlist}} objects or an overview of specific taxa
in such objects.}
\usage{
\S4method{summary}{taxlist}(object, ConceptID, units="Kb", validate=TRUE, display="both", maxsum=5, ...)
}
\arguments{
\item{object}{a taxlist object.}
\item{ConceptID}{character or integer value or vector with optional concept IDs
	for the summary.}
\item{units}{character value indicating the units used for the allocated space
	for object (see \code{\link{object.size}}).}
\item{validate}{logical value indicating whether 'object' should be validated
	or not}
\item{display}{character value indicating the field to be displayed (see
	details).}
\item{maxsum}{integer indicating the maximum number of displayed taxa.}
\item{...}{further arguments passed to or from another methods.}
}
\details{
A general overview indicating number of names, concepts and taxon views included
in a \code{\link[=taxlist-class]{taxlist}} object.

If argument \code{taxon} is an integer vector with concept IDs, then a display
of all names included in each concept will be produced.
Alternative you can use \code{taxon="all"} in order to get the listing of
names for all concepts included in the object.
The later is only recommended for small objects.

For summaries applied to concepts, there are three alternative displays of names
(argument \code{display}).
Use \code{display="name"} to show the value \code{TaxonName},
\code{display="author"} to show the value \code{AuthorName} or
\code{display="both"} to show both values.
Such values are taken from slot \code{taxonNames}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link[=taxlist-class]{taxlist}}.
}
\examples{
library(taxlist)
data(Easplist)

## summary of the object
summary(Easplist, units="Mb")

## summary for two taxa
summary(Easplist, c(51128,51140))

## summary for the first 10 taxa
summary(Easplist, "all", maxsum=10)
}
\keyword{methods}
