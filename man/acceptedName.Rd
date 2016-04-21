\name{acceptedName}
\alias{acceptedName}
\alias{acceptedName,taxlist-method}
\alias{acceptedName<-}
\alias{acceptedName<-,taxlist-method}

\title{Set Accepted Name for a Taxon Concept}
\description{
This function is written for actualization of nomenclature in taxlist objects
(i.e. species lists).
}
\usage{
acceptedName(taxlist, ConceptID) <- value
\S4method{acceptedName}{taxlist}(taxlist, ConceptID, ...)
}
\arguments{
\item{taxlist}{An object of class \dQuote{taxlist}.}
\item{ConceptID}{Corresponding concept ID where to set a new accepted name or
	first name.}
\item{value}{Usage ID of the new accepted name or first name, or new concept ID
	for a name.}
\item{...}{Further arguments to pass from or to methods.}
}
\details{
The function \code{acceptedName<-} changes the information contained in the slot
\code{taxonRelations} assigning the new \code{value} in the column
\code{AcceptedName}, which is supposed to be an usage ID.
For such procedure it is required that the usage \code{value} belongs to the
taxon concept for which it will be set as accepted name, otherwise the function
returns an error message.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\examples{
library(taxlist)
data(EAsplist)

## Assign first name to Cyclosorus interruptus
first_name(EAsplist, 50074) <- 53097
summary(EAsplist, 50074)

## set a new accepted name (the first name)
acceptedName(EAsplist, 50074) <- 53097
summary(EAsplist, 50074)
\donttest{
## attempt to use a name from another concept
acceptedName(EAsplist, 50074) <- 51129
}
}
