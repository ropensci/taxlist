\name{merge_taxa}
\docType{methods}
\alias{merge_taxa}
\alias{merge_taxa,taxlist,numeric-method}

\title{Merge concepts.}
\description{
Merge taxon concepts form a \code{\linkS4class{taxlist}} object into single
ones.
}
\usage{
\S4method{merge_taxa}{taxlist,numeric}(object, concepts, print_output=FALSE,
	...)
}
\arguments{
\item{object}{Object of class \code{\linkS4class{taxlist}}.}
\item{concepts}{Numeric (integer) vector including taxon concepts to be merged.}
\item{print_output}{Logical value indicating whether the merged concept should
	be displayed in the console.}
\item{...}{Further arguments to be passed to or from other methods.}
}
\details{
Taxon concepts indicated in argument \code{'concepts'} will be merged into a
single one.
The new concept inherites the ID and respective attributes from slots
\code{'taxonRelations'} and \code{'taxonTraits'} from the first taxon concept
indicated in argument \code{'concepts'}.

For convenience the resulting concept can be displayed by setting
\code{print_output=TRUE}.
}
\value{
An object of class \code{\linkS4class{taxlist}}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(taxlist)

data(Easplist)

## Merge Cyperus papyrus and Cyperus dives
summary(Easplist, c(206, 197))

Easplist <- merge_taxa(Easplist, c(206, 197), TRUE)
}
\keyword{methods}
