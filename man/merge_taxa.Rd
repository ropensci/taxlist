\name{merge_taxa}
\docType{methods}
\alias{merge_taxa}
\alias{merge_taxa,taxlist,numeric,missing-method}
\alias{merge_taxa,taxlist,missing,character-method}
\alias{change_cocept}
\alias{change_concept<-}
\alias{change_concept<-,taxlist-method}

\title{Merge Concepts}
\description{
Merge taxon concepts form a \code{\linkS4class{taxlist}} object into single
ones.
}
\usage{
\S4method{merge_taxa}{taxlist,numeric,missing}(object, concepts, level,
	print_output=FALSE, ...)

\S4method{merge_taxa}{taxlist,missing,character}(object, concepts, level, ...)

change_concept(taxlist, UsageID) <- value
}
\arguments{
\item{object,taxlist}{Object of class \code{\linkS4class{taxlist}}.}
\item{concepts}{Numeric (integer) vector including taxon concepts to be merged.}
\item{level}{Character vector indicating the lowest level for merging.}
\item{print_output}{Logical value indicating whether the merged concept should
	be displayed in the console.}
\item{UsageID}{Numeric vector with taxon usage IDs to be changed from concept.}
\item{value}{Numeric vector with taxon concept IDs to be assigned to the names.}
\item{...}{Further arguments to be passed to or from other methods.}
}
\details{
Taxon concepts indicated in argument \code{'concepts'} will be merged into a
single concept.
The new concept inherits the ID and respective attributes from slots
\code{'taxonRelations'} and \code{'taxonTraits'} from the first taxon concept
indicated in argument \code{'concepts'}.

For convenience the resulting concept can be displayed by setting
\code{'print_output=TRUE'} but only when using argument \code{'concepts'}.

An alternative application of this function is implemented through the argument
\code{'level'}, where all lower rank taxa will be merged to the indicated level
or higher (if parent of merged taxa are at a higher rank).
}
\value{
An object of class \code{\linkS4class{taxlist}}.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\examples{
library(taxlist)
data(Easplist)

## Merge Cyperus papyrus and Cyperus dives
summary(Easplist, c(206, 197))

Easplist <- merge_taxa(Easplist, c(206, 197), print_output=TRUE)

## Move the name Typha aethiopica to concept 573 (T. latifolia)
change_concept(Easplist, 53130) <- 573
summary(Easplist, c(50105,573))
\donttest{
## Attempting to move an accepted name
change_concept(Easplist, 50105) <- 573
}
}
\keyword{methods}
