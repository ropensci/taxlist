\name{taxon_views}
\docType{methods}
\alias{taxon_views}
\alias{taxon_views,taxlist-method}
\alias{taxon_views<-}
\alias{taxon_views<-,taxlist,data.frame-method}

\title{Management of Concept Views in Taxonomic Lists.}
\description{
Retrieve or replace slot \code{taxonViews} in an object of class
\code{\link[=taxlist-class]{taxlist}}.
}
\usage{
\S4method{taxon_views}{taxlist}(taxlist, ...)

taxon_views(taxlist) <- value
}
\arguments{
\item{taxlist}{object of class \code{data.frame} or \code{taxlist}.}
\item{value}{a \code{data.frame} containing information about concept views
	implemented.}
\item{...}{Further arguments passed to or from other methods.}
}
\details{
The function \code{taxon_views} retrieves the slot \code{taxonViews} from
objects of the class \code{\link[=taxlist-class]{taxlist}}.

Two replacement methods are implemented.
If \code{value} is an integer vector, it is assumed to indicate the respective
view for each concept included in the slot \code{taxonRelations}.
If \code{value} is a data frame, then it will be used to replace the slot
\code{taxonViews}.

In \code{add_view}, \code{...} represent additional variables to be inserted as
columns in slot \code{taxonViews}.

The term \sQuote{taxon view} was introduced by \bold{Zhong et al. (1996)} and
corresponds to the reference used for the definition of a concept.
Objects of class \code{\link[=taxlist-class]{taxlist}} use only a
\sQuote{primary taxon views} and do not allow \sQuote{secondary} or
\sQuote{parent-child taxon views}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\references{
\bold{Zhong Y, Jung S, Pramanik S, Beaman JH (1996).} Data model and comparison
and query methods for interacting classifications in a taxonomic database.
\emph{Taxon} 45: 223--241.
}


\examples{
library(taxlist)
data(Easplist)

taxon_views(Easplist)
}
\keyword{methods}
