\name{df2taxlist}
\docType{methods}
\alias{df2taxlist}
\alias{df2taxlist,data.frame,logical-method}

\title{Convert a Data Frame into a taxlist Object}
\description{
Conversion of a data frame containing species lists into a taxlist object.
}
\usage{
df2taxlist(x, AcceptedName, ...)
}
\arguments{
\item{x}{A data frame formatted for a taxlist object.}
\item{AcceptedName}{A logical vector indicating accepted names with value
	\code{TRUE}.}
\item{...}{Further arguments to be passed from or to other methods.}
}
\details{
This function coerces a data frame containing a species list into an object of
class \code{\link{taxlist}}. The input data frame \code{x} must have following
columns:
\describe{
	\item{TaxonUsageID}{Numeric code for the name.}
	\item{TaxonConceptID}{Numeric code for the concept.}
	\item{TaxonName}{Full name (usage), excluding author name.}
	\item{AuthorName}{Author of the combination (taxon name).}
}
You may additionally insert taxon views to the resulting object
(see \code{\link{taxon_views}}).
}
\value{
An object of class \code{\link[=taxlist-class]{taxlist}}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
## Two files are included in the package installation
library(taxlist)
setwd(file.path(path.package("taxlist"), "cyperus"))

## Read the table with names of Cyperus species
Cyperus <- read.csv("names.csv", stringsAsFactors=FALSE)
head(Cyperus)

## Convert to 'taxlist' object
Cyperus <- df2taxlist(Cyperus, AcceptedName=!Cyperus$SYNONYM)
summary(Cyperus)
}
\keyword{methods}
