\name{add_parent}
\docType{methods}
\alias{add_parent}
\alias{add_parent,taxlist-method}
\alias{add_parent<-}
\alias{add_parent<-,taxlist-method}

\title{Add or modify parent entries in single concepts.}
\description{
Description.
}
\usage{
\S4method{add_parent}{taxlist}(taxlist, ConceptID, Parent, ...)

add_parent(taxlist, ConceptID) <- value
}
\arguments{
\item{taxlist}{a \code{\link[=taxlist-class]{taxlist}} object.}
\item{ConceptID}{concept IDs where to add level information.}
\item{Parent,value}{new value of level for the respective concepts.}
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
