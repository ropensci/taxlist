\name{get_children}
\docType{methods}
\alias{get_children}
\alias{get_children,taxlist-method}
\alias{get_parents}
\alias{get_parents,taxlist-method}

\title{Retrieve children or parents of requested concepts.}
\description{
Describe.
}
\usage{
\S4method{get_children}{taxlist}(taxlist, ConceptID, ...)

\S4method{get_parents}{taxlist}(taxlist, ConceptID, ...)
}
\arguments{
\item{taxlist}{An object of class \code{\link[=taxlist-class]{taxlist}}.}
\item{ConceptID}{Concept IDs for selecting parents or children.}
\item{...}{Further arguments to pass from or to other methods.}
}
\details{
Details
}
\value{
A \code{\link[=taxlist-class]{taxlist}} object with a subset including requested
concepts with children or parents.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\examples{
## Add example
}
\keyword{methods}
