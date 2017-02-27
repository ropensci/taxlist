\name{add_level}
\docType{methods}
\alias{add_level}
\alias{add_level,taxlist-method}
\alias{add_level<-}
\alias{add_level<-,taxlist-method}

\title{Add or modify level entries in single concepts.}
\description{
Description.
}
\usage{
\S4method{add_level}{taxlist}(taxlist, ConceptID, Level, ...)

add_level(taxlist, ConceptID) <- value
}
\arguments{
\item{taxlist}{a \code{\link[=taxlist-class]{taxlist}} object.}
\item{ConceptID}{concept IDs where to add level information.}
\item{Level,value}{new value of level for the respective concepts.}
\item{...}{further arguments to pass from or to other methods.}
}
\details{
Details
}
\value{
An object of class \code{\link[=taxlist-class]{taxlist}} with modified entries.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
## Add example
}
\keyword{methods}
