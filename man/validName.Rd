% multi-purpose template
\name{validName}
\alias{validName}
\alias{validName,taxlist-method}
\alias{validName<-}
\alias{validName<-,taxlist-method}

\title{Set Valid Name for a Taxon Concept}
\description{
This function is written for actualization of nomenclature in taxlist objects
(i.e. species lists).
}
\usage{
validName(taxlist, ConceptID) <- value
\S4method{validName}{taxlist}(taxlist, ConceptID, ...)
}
\arguments{
	\item{taxlist}{An object of class \dQuote{taxlist}.}
	\item{ConceptID}{Corresponding concept ID where to set a new valid name or
		first name.}
	\item{value}{Usage ID of the new valid name or first name, or new concept ID
		for a name.}
	\item{...}{Further arguments to pass from or to methods.}
}
\details{
The function \code{validName<-} changes the information contained in the slot
\code{taxonRelations} assigning the new \code{value} in the column
\code{ValidName}, which is supposed to be an usage ID.
For such procedure it is required that the usage \code{value} belongs to the
taxon concept for which it will be set as valid name, otherwise the function
returns an error message.
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\examples{
library(taxlist)
data(conosur_splist)

## assign first name to Pseudognaphalium cymatoides
first_name(conosur_splist, 75144) <- 70906
summary(conosur_splist, 75144)

## set a new valid name (the first name)
validName(conosur_splist, 75144) <- 70906
summary(conosur_splist, 75144)
\donttest{
## attempt to use a name from another concept
validName(conosur_splist, 75144) <- 70903
}
}
