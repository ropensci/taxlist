\name{add_level}
\docType{methods}
\alias{add_level}
\alias{add_level,taxlist-method}
\alias{add_level<-}
\alias{add_level<-,taxlist-method}

\title{Add or modify level entries in single concepts.}
\description{
Add taxonomic levels for specific taxon concepts in a
\code{\linkS4class{taxlist}} object.
Also changes in concept circumscription may implicate changes in its taxonomic
hierarchy.
}
\usage{
\S4method{add_level}{taxlist}(taxlist, ConceptID, Level, ...)

add_level(taxlist, ConceptID) <- value
}
\arguments{
\item{taxlist}{A \code{\linkS4class{taxlist}} object.}
\item{ConceptID}{Concept IDs where to add level information.}
\item{Level,value}{New value of level for the respective concepts.}
\item{...}{Further arguments to pass from or to other methods.}
}
\details{
Taxonomic levels to add in concepts have to be previously declared for the
\code{\linkS4class{taxlist}} object in the input (see
\code{\link[taxlist]{levels}}).
If the level is not previously set for the object, an error message will be
retrieved.
}
\value{
An object of class \code{\linkS4class{taxlist}} with modified level
entries.
}
\seealso{
\code{\link[taxlist]{levels}}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
## No example at the moment
}
\keyword{methods}
