\name{summary}
\docType{methods}
\alias{summary}
\alias{summary,taxlist-method}

\title{Print Overviews for taxlist Objects and their Content}
\description{
A method to display either an overview of the content of
\code{\linkS4class{taxlist}} objects or an overview of selected taxa.
}
\usage{
\S4method{summary}{taxlist}(object, ConceptID, units="Kb", check_validity=TRUE,
	display="both", maxsum=5, secundum=NULL, ...)
}
\arguments{
\item{object}{A \code{\linkS4class{taxlist}} object.}
\item{ConceptID}{IDs of concepts to be displayed in the summary.}
\item{units}{Character value indicating the units shown in the object's
	allocated space.}
\item{check_validity}{Logical value indicating whether the validity of
	\code{'object'} should be checked or not}
\item{display}{Character value indicating the field to be displayed (see
	details).}
\item{maxsum}{Integer indicating the maximum number of displayed taxa.}
\item{secundum}{A character value indicating the column from slot
	\code{'taxonViews'}} to be displayed in the summary.
\item{...}{Further arguments passed to or from another methods.}
}
\details{
A general overview indicating number of names, concepts and taxon views included
in \code{\linkS4class{taxlist}} objects.
If argument \code{'ConceptID'} is a vector with concept IDs or names to be
matched by \code{\link[=grepl]{grepl()}}, then a display
of all names included in each concept will be produced.
Alternative you can use \code{taxon="all"} in order to get the listing of
names for all concepts included in the object (truncated to the input number of
\code{'maxsum'}).

For summaries applied to concepts, there are three alternative displays of names
using the argument \code{'display'}.
Use \code{display="name"} to show the value \code{'TaxonName'},
\code{display="author"} to show the value \code{'AuthorName'} or
\code{display="both"} to show both values.
Such values are taken from slot \code{'taxonNames'}.

For big objects it will be recommendet to set \code{units="Mb"} (see also
\code{\link[=object.size]{object.size()}} for further alternatives).
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\seealso{
\code{\linkS4class{taxlist}}.
}
\examples{
library(taxlist)
data(Easplist)

## summary of the object
summary(Easplist, units="Mb")

## summary for two taxa
summary(Easplist, c(51128,51140))

## summary for a name
summary(Easplist, "Acmella")

## summary for the first 10 taxa
summary(Easplist, "all", maxsum=10)
}
\keyword{methods}
