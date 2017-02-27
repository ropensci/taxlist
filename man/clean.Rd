\name{clean}
\docType{methods}
\alias{clean}
\alias{clean,taxlist-method}

\title{Insert a New Concept in a Taxonomic List.}
\description{
Eliminates non-linked information from slots \code{taxonNames},
\code{taxonTraits} and \code{taxonViews}.
}
\usage{
\S4method{clean}{taxlist}(object, ...)
}
\arguments{
\item{object}{An object of class \code{\link[=taxlist-class]{taxlist}}.}
\item{...}{Additional named vectors to be included in the slots 
	\code{taxonNames} or \code{taxonRelations}.}
}
\details{
Details.
}
\value{
A clean \code{\link[=taxlist-class]{taxlist}} object.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
## Add an example
}
\keyword{methods}
