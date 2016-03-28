\name{summary}
\docType{methods}
\alias{summary}
\alias{summary,taxlist-method}

\title{Print Overview of \dQuote{taxlist} Objects}
\description{
A method to display either an overview of the content of
\code{"\link[=taxlist-class]{taxlist}"} objects or an overview of specific taxa
in such objects.}
\usage{
\S4method{summary}{taxlist}(object, taxon, display="both", validate=TRUE)
}
\arguments{
\item{object}{a taxlist object}
\item{taxon}{a character or integer value or vector with optional concept IDs
	for the summary}
\item{display}{character value indicating the field to be displayed (see
	details)}
\item{validate}{logical value indicating whether 'object' should be validated
	or not}
}
\details{
Review the description

Link to validObject!!! taxon can be missing

Both methods are applied to \code{"\link[=taxlist-class]{taxlist}"} objects. The
\code{taxlist} method provides an overview on the content of such an object,
while the \code{character} method provides an option to display an overview of
single taxa included in the \code{taxlist} object.

In the \code{taxlist} method a validation of the object will be carried out by
\code{\link{validObject}}.

In the \code{character} method the vector \code{x} should contain identifiers
corresponding to the variable \code{TaxonUsageID} (see
\code{"\link[=taxlist-class]{taxlist}"}).
For each taxon a display of the valid name, first name (if set for the taxon) and
synonyms (excluding valid name) will be shown in the console.
Every name will be preceded by its \code{TaxonUsageID}.
The argument \code{display} indicates which fields are displayed in the console,
either the field \code{TaxonName} (\code{display="name"}), \code{AuthorName}
(\code{display="author"}), or both (\code{display="both"}).
The option \code{"both"} is included for lists with a field \code{AuthorName}
combining taxon's and author's names in one string.
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\examples{
library(taxlist)
data(conosur_splist)

## summary of the object
summary(conosur_splist)

## summary for two taxa
summary(conosur_splist,c(193729,75144))
}
