\name{first_name}
\alias{first_name}
\alias{first_name,taxlist-method}
\alias{first_name<-}
\alias{first_name<-,taxlist-method}

\title{Retrieve or Set First Name for a Concept}
\description{
Function handling first names for taxon concepts. 
}
\usage{
first_name(taxlist, ConceptID) <- value
\S4method{first_name}{taxlist}(taxlist, ConceptID, ...)
}
\arguments{
\item{taxlist}{An object of class \dQuote{taxlist}.}
\item{ConceptID}{An integer vector with corresponding concepts IDs where to set
	first name.}
\item{value}{An integer vector with the respective usage IDs to be used as first
	names.}
\item{...}{Further arguments to pass from or to other methods.}
}
\details{
Function \code{first_name} retrieves the usage ID for a requested concept.
The application \code{first_name<-} will change the information contained in the
slot \code{taxonRelations} assigning the new \code{value} in the columns
\code{first_name}, which is supposed to be a usage ID.
For this change the \code{value} must be included as usage for the concept
(\code{ConceptID}) that will use it as first name, otherwise the function returns
an error message.

This function may also support vectors for the arguments \code{ConceptID} and
\code{value}.
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\examples{
library(taxlist)
data(EAsplist)

## Missing first name for Pseudognaphalium cymatoides
summary(EAsplist, 50074)

## Assign first name
first_name(EAsplist, 50074) <- 53097
summary(EAsplist, 50074)

## Retrieve first name
first_name(EAsplist, 50074)
}
