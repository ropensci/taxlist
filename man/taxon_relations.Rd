\name{taxon_relations}
\docType{methods}
\alias{taxon_relations}
\alias{taxon_relations,taxlist-method}
\alias{taxon_relations<-}
\alias{taxon_relations<-,taxlist,data.frame-method}

\title{Retrieve or replace Slot \sQuote{taxonRelations} in \sQuote{taxlist}
objects}
\description{
Retrieve the content of slot \code{'taxonRelations'} from a
\code{\linkS4class{taxlist}} object or replace it by a new data frame.
}
\usage{
\S4method{taxon_relations}{taxlist}(taxlist, ...)

taxon_relations(taxlist) <- value
}
\arguments{
\item{taxlist}{A \code{\linkS4class{taxlist}} object.}
\item{value}{A \code{'data.frame'} object to be set as slot
	\code{'taxonRelations'}.}
\item{...}{Further arguments passed to or from other methods.}
}
\details{
The replacement method \code{'taxon_relatiions<-'} should be only used when
constructing \code{\linkS4class{taxlist}} objects from an empty one (prototype).
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
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
taxon_relations(Euclea)
}
\keyword{methods}
