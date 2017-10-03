\name{tax2traits}
\docType{methods}
\alias{tax2traits}
\alias{tax2traits,taxlist-method}

\title{Add taxonomic information to slot 'taxonTraits'.}
\description{
Taxonomic classification can be included in \code{\linkS4class{taxlist}}
objects.
Nevertheless for some analyses it may be more convenient to insert such
information in the slot \code{'taxonTraits'}.
}
\usage{
\S4method{tax2traits}{taxlist}(object, ...)
}
\arguments{
\item{object}{An object of class \code{\linkS4class{taxlist}}.}
\item{...}{Further arguments to be passed among methods.}
}
\details{
This function can only be applied to objects containing parent-child
relationships and information on taxonomic levels.
}
\value{
An object of class \code{\linkS4class{taxlist}} with taxonomy added as traits.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link{add_trait}}.
}
\examples{
# TODO: Add example
}

