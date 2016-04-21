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
	\item{TaxonUsageID}{Code for the name.}
	\item{TaxonConceptID}{Code for the taxon.}
	\item{TaxonName}{Full name of taxon.}
	\item{AuthorName}{Author of the combination (taxon name).}
}
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\examples{
}
