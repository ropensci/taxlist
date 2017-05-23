\name{df2taxlist}
\docType{methods}
\alias{df2taxlist}
\alias{df2taxlist,data.frame,logical-method}
\alias{df2taxlist,data.frame,missing-method}
\alias{df2taxlist,character,missing-method}

\title{Convert a Data Frame into a taxlist Object}
\description{
Conversion of a data frame containing species lists into a taxlist object.
}
\usage{
\S4method{df2taxlist}{data.frame,logical}(x, AcceptedName, ...)

\S4method{df2taxlist}{data.frame,missing}(x, AcceptedName, ...)

\S4method{df2taxlist}{character,missing}(x, AcceptedName, ...)

}
\arguments{
\item{x}{A data frame formatted for a taxlist object.}
\item{AcceptedName}{A logical vector indicating accepted names with value
	\code{TRUE}.}
\item{...}{Named vectors to be added as columns in slot \code{'taxonNames'}.}
}
\details{
This function coerces a data frame containing a species list into an object of
class \code{\linkS4class{taxlist}}.
In the method \code{'data.frame'}, the input data frame must have following
columns:
\describe{
	\item{TaxonUsageID}{Numeric code for the name.}
	\item{TaxonConceptID}{Numeric code for the concept.}
	\item{TaxonName}{Full name (usage), excluding author name.}
	\item{AuthorName}{Author of the combination (taxon name).}
}

Be aware that the resulting object misses any information on taxon views,
basionyms, parent concepts, hierarchical levels and taxon traits.
All those elements can be added \emph{a posteriori} by further functions
provided in this package.

By using the \code{'character'} method, all provided names will be assumed as
accepted ones.
}
\value{
A \code{\linkS4class{taxlist} object}.
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

## Create a 'taxlist' object from character vectors
Plants <- df2taxlist(c("Triticum aestivum","Zea mays"), AuthorName="L.")
summary(Plants, "all")
}
\keyword{methods}
