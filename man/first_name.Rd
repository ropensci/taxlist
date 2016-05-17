\name{first_name}
\docType{methods}
\alias{first_name}
\alias{first_name,taxlist-method}
\alias{first_name<-}
\alias{first_name<-,taxlist-method}

\title{Retrieve or Set First Name for a Concept.}
\description{
Function handling first names for taxon concepts. 
}
\usage{
\S4method{first_name}{taxlist}(taxlist, ConceptID, ...)

first_name(taxlist, ConceptID) <- value
}
\arguments{
\item{taxlist}{An object of class \code{\link[=taxlist-class]{taxlist}}.}
\item{ConceptID}{An integer vector with corresponding concepts IDs where to set
	first names.}
\item{value}{An integer vector with the respective usage IDs to be used as first
	names.}
\item{...}{Further arguments to pass from or to other methods.}
}
\details{
Function \code{first_name} retrieves the usage ID for a requested concept.
The replacement method will change the information contained in
the slot \code{taxonRelations} assigning the new \code{value} in the columns
\code{first_name}, which is supposed to be a usage ID.
For this change the \code{value} must be included as usage for the concept
(\code{ConceptID}) that will use it as first name, otherwise the function
returns an error message.
}
\value{
An integer vector with the retrieved IDs or an object of class
\code{\link[=taxlist-class]{taxlist}} with modified entries.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\examples{
library(taxlist)
data(Easplist)

## Missing first name for Cyclosorus interruptus
summary(Easplist, 50074)

## Assign first name
first_name(Easplist, 50074) <- 53097
summary(Easplist, 50074)

## Retrieve first name
first_name(Easplist, 50074)
}
\keyword{methods}
