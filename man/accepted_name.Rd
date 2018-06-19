\name{accepted_name}
\docType{methods}
\alias{accepted_name}
\alias{accepted_name,taxlist,numeric-method}
\alias{accepted_name,taxlist,missing-method}
\alias{accepted_name<-}
\alias{accepted_name<-,taxlist,numeric,numeric-method}
\alias{synonyms}
\alias{synonyms,taxlist,numeric-method}
\alias{synonyms,taxlist,missing-method}
\alias{basionym}
\alias{basionym,taxlist,numeric-method}
\alias{basionym,taxlist,missing-method}
\alias{basionym<-}
\alias{basionym<-,taxlist,numeric,numeric-method}

\title{Manage Accepted Names, Synonyms and Basionyms}
\description{
Taxon usage names for a taxon concept can be divided into three categories:
accepted names, basionyms and synonyms.
Each single taxon concept may at least have an accepted name, while basionym
and synonyms are optional.
The functions \code{accepted_name()}, \code{basionym()} and \code{synonyms()}
can be used either to display the respective usage names or to set usage names
in one of those categories.
}
\usage{
\S4method{accepted_name}{taxlist,numeric}(taxlist, ConceptID, show_traits=FALSE, ...)

\S4method{accepted_name}{taxlist,missing}(taxlist, ConceptID, ...)

## Replacement method
accepted_name(taxlist, ConceptID) <- value

\S4method{synonyms}{taxlist,numeric}(taxlist, ConceptID, ...)

\S4method{synonyms}{taxlist,missing}(taxlist, ConceptID, ...)

\S4method{basionym}{taxlist,numeric}(taxlist, ConceptID, ...)

\S4method{basionym}{taxlist,missing}(taxlist, ConceptID, ...)

## Replacement method
basionym(taxlist, ConceptID) <- value
}
\arguments{
\item{taxlist}{An object of class \code{\linkS4class{taxlist}}.}
\item{ConceptID}{Integer containing concept IDs where to request or set
	names for one category.}
\item{show_traits}{Logical value, whether traits should be included in the
	output of \code{accepted_name()} or not.}
\item{value}{Integer containing usage IDs to be set to the respective category
	in the respective taxon concept.}
\item{...}{Further arguments passed among methods.}
}
\details{
The function \code{accepted_name()} retrieves the accepted names for the
indicated taxon concepts or for the whole \code{\linkS4class{taxlist}} object.
By using \code{'show_traits=TRUE'}, the respective taxon traits will be
displayed as well, providing an overview of taxa included in the object.
The replacement method for this function will set the respective usage name IDs
as accepted names for the respective taxon concept, provided that these names
are already set as synonyms in the respective concepts.

The function \code{synonyms()} is working in a similar way as
\code{accepted_name()}, but this function does not include taxon traits in the
output and there is no replacing method for \code{synonyms()}.
Alternatives for inserting new synonyms into a taxon concept are either moving
synonyms from other taxa by using
\code{\link{change_concept<-}} or inserting
new names in the object by using \code{\link[=add_synonym]{add_synonym()}}.

The function \code{basionym()} is retrieving and setting basionyms in the
respective taxon concepts similarly to \code{accepted_name()}, but this function
does not retrieve any information on taxon traits, either.
}
\value{
Most of the methods return information in data frames, while replacement methods
do it as \code{\linkS4class{taxlist}} objects.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\seealso{
\code{\link{add_synonym}}, \code{\link{change_concept<-}}.
}
\examples{
library(taxlist)
data(Easplist)

## Set a different accepted name for Cyclosorus interruptus
summary(Easplist, "Cyclosorus interruptus")
accepted_name(Easplist, 50074) <- 53097
summary(Easplist, 50074)

## Inserting a new name first
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
