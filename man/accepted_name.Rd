\name{accepted_name}
\alias{accepted_name}
\alias{accepted_name,taxlist-method}
\alias{accepted_name<-}
\alias{accepted_name<-,taxlist-method}

\title{Retrieve or Set Accepted Name for a Taxon Concept.}
\description{
This function is written for actualization of nomenclature in
\code{"\link[=taxlist-class]{taxlist}"} objects.
}
\usage{
\S4method{accepted_name}{taxlist}(taxlist, ConceptID, ...)

accepted_name(taxlist, ConceptID) <- value
}
\arguments{
\item{taxlist}{An object of class \code{"\link[=taxlist-class]{taxlist}"}.}
\item{ConceptID}{Corresponding concept ID where to request or set an accepted
	name.}
\item{value}{Usage ID of the name to set as the accepted one.}
\item{...}{Further arguments to pass from or to methods.}
}
\details{
The function \code{accepted_name<-} changes the information contained in the slot
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
data(Easplist)

## Assign first name to Cyclosorus interruptus
first_name(Easplist, 50074) <- 53097
summary(Easplist, 50074)

## set a new accepted name (the first name)
accepted_name(Easplist, 50074) <- 53097
summary(Easplist, 50074)
\donttest{
## attempt to use a name from another concept
accepted_name(Easplist, 50074) <- 51129
}
}
