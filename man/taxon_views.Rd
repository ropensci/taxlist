\name{taxon_views}
\docType{methods}
\alias{taxon_views}
\alias{taxon_views,taxlist-method}
\alias{taxon_views<-}
\alias{taxon_views<-,taxlist,data.frame-method}

\title{Management of concept views in taxonomic lists.}
\description{
Retrieve or replace slot \code{taxonViews} in an object of class
\code{\linkS4class{taxlist}}.
}
\usage{
\S4method{taxon_views}{taxlist}(taxlist, ...)

taxon_views(taxlist) <- value
}
\arguments{
\item{taxlist}{A \code{\linkS4class{taxlist}} object.}
\item{value}{An object of class \code{'data.frame'} containing the references
	used to define the circumscription of taxon concepts included in
	\code{'taxlist'}.}
\item{...}{Further arguments passed to or from other methods.}
}
\details{
The term \sQuote{taxon view} was introduced by \bold{Zhong et al. (1996)} and
corresponds to the reference used for the definition of a concept.

This function retrieves the slot \code{'taxonViews'} from objects of the class
\code{\linkS4class{taxlist}}.

The replacement method \code{'taxon_views<-'} replaces the whole content of slot
\code{'taxonViews'} and it is only recommended to use when constructing a new
\code{\linkS4class{taxlist}} object from an empty prototype.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\references{
\bold{Zhong Y, Jung S, Pramanik S, Beaman JH (1996).} Data model and comparison
and query methods for interacting classifications in a taxonomic database.
\emph{Taxon} 45: 223--241.
}
\seealso{
\code{\link{add_view}}.
}
\examples{
library(taxlist)
data(Easplist)

taxon_views(Easplist)
}
\keyword{methods}
