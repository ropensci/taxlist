\name{accepted_name}
\docType{methods}
\alias{accepted_name}
\alias{accepted_name,taxlist-method}
\alias{accepted_name<-}
\alias{accepted_name<-,taxlist-method}

\title{Retrieve or Set Accepted Name for a Taxon Concepts.}
\description{
Changes on accepted names for a concept.
}
\usage{
\S4method{accepted_name}{taxlist}(taxlist, ConceptID, ...)

accepted_name(taxlist, ConceptID) <- value
}
\arguments{
\item{taxlist}{an object of class \code{\link[=taxlist-class]{taxlist}}.}
\item{ConceptID}{integer vector containing concept IDs where to request or set
	an accepted name.}
\item{value}{integer vector containing usage IDs to be set as accepted names.}
\item{...}{further arguments passed from or to other methods.}
}
\details{
The method \code{accepted_name<-} changes the information contained in the slot
\code{taxonRelations} assigning the new \code{value} in the column
\code{AcceptedName}, which is supposed to be an usage ID.
For such procedure \code{value} must belong to the concept, where it will be
set as new accepted name, otherwise the function will return an error message.

In the arguments \code{ConcpetID} and \code{value} also vectors can be used,
then they may have of the same length.
}
\value{
The method \code{accepted_name} retrieves a subset of the slot \code{taxonNames}
as \code{data.frame} object, while the replacement method \code{accepted_name<-}
results in a \code{\link[=taxlist-class]{taxlist}} object with modified
\sQuote{accepted name} information.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(taxlist)
data(Easplist)

## Overview of Cyclosorus interruptus
summary(Easplist, 50074)

## set a new accepted name (the first name)
accepted_name(Easplist, 50074) <- 53097
summary(Easplist, 50074)
\donttest{
## attempt to use a name from another concept
accepted_name(Easplist, 50074) <- 51129
}
}
\keyword{methods}
