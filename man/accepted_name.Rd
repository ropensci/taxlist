\name{accepted_name}
\docType{methods}
\alias{accepted_name}
\alias{accepted_name,taxlist,numeric-method}
\alias{accepted_name,taxlist,missing-method}
\alias{accepted_name<-}
\alias{accepted_name<-,taxlist,numeric,numeric-method}

\title{Retrieve or set accepted name for taxon concepts.}
\description{
Changes on accepted names for a taxon concept may in some cases not affect the
circumscription of the taxon.
In such cases the current accepted name will be set as synonym, while as
accepted name either an existing synonym or a new name should be set as new
accepted name.
}
\usage{
\S4method{accepted_name}{taxlist,numeric}(taxlist, ConceptID, get_traits=FALSE,
	...)

\S4method{accepted_name}{taxlist,missing}(taxlist, ConceptID, ...)

accepted_name(taxlist, ConceptID) <- value
}
\arguments{
\item{taxlist}{An object of class \code{\linkS4class{taxlist}}.}
\item{ConceptID}{Integer containing concept IDs where to request or set
	accepted names.}
\item{get_traits}{Logical value, whether traits should be included in output or
	not.}
\item{value}{Integer containing usage IDs to be set as new accepted names.}
\item{...}{Further arguments passed from or to other methods.}
}
\details{
This function retrieves or replace the usage ID indicating the accepted name for
a taxon concept.
With the method \code{'accepted_name<-'} a new accepted name for the respective
taxon concept will be set, provided that this name is already considered as
synonym for the taxon, otherwise an error message will be retrieved.

The vectors used in the arguments \code{'ConceptID'} and \code{'value'} can be
either of class \code{'numeric'} or \code{'integer'} and both vectors have to be
of the same length for multiple replacements.
}
\value{
The method \code{'accepted_name'} retrieves a \code{'data.frame'} object
indicating the identity of the concept and the name and including the accepted
taxon name and the respective author.

The replacement method \code{'accepted_name<-'} results in a
\code{\linkS4class{taxlist}} object with modified entries for accepted names.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link{add_synonym}}.
}
\examples{
library(taxlist)
data(Easplist)

## Overview of Cyclosorus interruptus
summary(Easplist, "Cyclosorus interruptus")

## Set a new accepted name (the oldest name)
accepted_name(Easplist, 50074) <- 53097
summary(Easplist, 50074)

## When a name is not in the list
## It should be added first as synonym
summary(Easplist, "Basella alba")

Easplist <- add_synonym(Easplist, 68, TaxonName="Basella cordifolia",
	AuthorName="Lam.")
summary(Easplist, 68)
accepted_name(Easplist, 68) <- 56139
summary(Easplist, 68)

\donttest{
## attempt to use a name from another concept
accepted_name(Easplist, 50074) <- 51129
}
}
\keyword{methods}
