\name{taxon_names}
\docType{methods}
\alias{taxon_names}
\alias{taxon_names,taxlist-method}
\alias{taxon_names<-}
\alias{taxon_names<-,taxlist,data.frame-method}
\alias{update_name}
\alias{update_name,taxlist,numeric-method}

\title{Retrieve or replace slot \sQuote{taxonNames} in \sQuote{taxlist}
objects.}
\description{
The slot \code{'taxonNames'} in \code{\linkS4class{taxlist}} objects contains
the names that are used as accepted names and synonyms for the respective taxon
concepts.
This function offers a quick access to the content in the mentioned slot.
}
\usage{
\S4method{taxon_names}{taxlist}(taxlist, ...)

taxon_names(taxlist) <- value

\S4method{update_name}{taxlist,numeric}(object, UsageID, ...)
}
\arguments{
\item{taxlist,object}{A \code{\linkS4class{taxlist}} object.}
\item{value}{Object of class \code{'data.frame'} to set as slot
	\code{taxonNames}.}
\item{UsageID}{Numeric vector indicating the taxon usage IDs to be updated.}
\item{...}{Further arguments passed among methods. In \code{'update_name'} are
	vectors including the variables to be updated for the respective taxon usage
	ID.}
}
\details{
The replacement method \code{'taxon_names<-'} is a fast way to include names in
empty \code{\linkS4class{taxlist}} objects.
}
\value{
An object of class \code{'data.frame'} or, in the case of the replacement
method, a \code{\linkS4class{taxlist}} object with modified slot
\code{'taxonNames'}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\linkS4class{taxlist}}.
}
\examples{
library(taxlist)
data(Easplist)

## Subset for the genus Euclea and display of slot 'taxonNames'
Euclea <- subset(Easplist, charmatch("Euclea", TaxonName), slot="names")
Euclea <- get_children(Easplist, Euclea)

summary(Euclea)
taxon_names(Euclea)
}
\keyword{methods}
