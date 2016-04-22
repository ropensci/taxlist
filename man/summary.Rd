\name{summary}
\docType{methods}
\alias{summary}
\alias{summary,taxlist-method}

\title{Print Overview of \dQuote{taxlist} Objects}
\description{
A method to display either an overview of the content of
\code{\link[=taxlist-class]{taxlist}} objects or an overview of specific taxa
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
A general overview of objects or a detailed listing of alternative names for
single concepts.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\examples{
library(taxlist)
data(Easplist)

## summary of the object
summary(Easplist)

## summary for two taxa
summary(Easplist, c(51128,51140))
}
