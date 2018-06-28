\name{match_names}
\docType{methods}
\alias{match_names}
\alias{match_names,character,taxlist-method}

\title{Search Matchings between Character and taxlist Objects}
\description{
Names provided in a character vector will be compared with names stored in slot
\code{'taxonNames'} within an object of class \code{\linkS4class{taxlist}} by
using the function \code{\link{stringsim}}.
}
\usage{
\S4method{match_names}{character,taxlist}(x, object, clean=TRUE,
	output="data.frame", best=5, show_concepts=FALSE, method="lcs", ...)
}
\arguments{
\item{x}{A character vector with names to be compared.}
\item{object}{An object of class \code{\linkS4class{taxlist}} to be compared
	with.}
\item{clean}{Logical value, whether leading, tailing and double blanks should be
	deleted from \code{'x'}.}
\item{output}{Character value indicating the type of output (see details).}
\item{best}{Integer value indicating how many from the best matches have to be
	displayed (only working for \code{output="list"}).}
\item{show_concepts}{Logical value, whether respective concepts should be
	displayed in output or not.}
\item{method,...}{Further arguments passed to
	\code{\link[stringdist]{stringsim}}.}
}
\details{
For \code{output="list"} a list with the best matches (taxon usage name ID and
similarity) for each queried name will be retrieved, where the number is set by
argument \code{'best'}.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\seealso{
\code{\link[stringdist]{stringsim}}.
}
\examples{
library(taxlist)
data(Easplist)

## Names to be compared
species <- c("Cperus papyrus", "Typha australis", "Luke skywalker")

## Retrieve taxon usage names
match_names(species, Easplist)

## Display accepted names in output
match_names(species, Easplist, show_concepts=TRUE)
}
\keyword{methods}
