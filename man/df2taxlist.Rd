% multi-purpose template
\name{df2taxlist}
\alias{df2taxlist}

\title{Convert a Data Frame into a taxlist Object}
\description{
Conversion of a data frame containing species lists into a taxlist object.
}
\usage{
df2taxlist(x, AcceptedName, FirstName, Traits)
}
\arguments{
\item{x}{A data frame formatted for a taxlist object.}
\item{AcceptedName}{A logical vector indicating accepted names with value
	\code{TRUE}.}
\item{FirstName}{A logical vector indicating first names with value
	\code{TRUE}.}
\item{Traits}{A data frame with traits for included concepts.}
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
}
\value{
An object of class \code{\link[=taxlist-class]{taxlist}}.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\examples{
\donttest{
## Two files are included in the package installation
library(taxlist)
setwd(file.path(path.package("taxlits"), "cyperus"))

## Read the table with names of Cyperus species
Cyperus <- read.csv("names.csv")
head(Cyperus)

## Convert to 'taxlist' object
Cyperus <- df2taxlist(Cyperus)
summary(Cyperus)
}
}
