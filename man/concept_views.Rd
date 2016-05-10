\name{concept_views}
\docType{methods}
\alias{add_view}
\alias{concept_views}
\alias{concept_views,taxlist-method}
\alias{concept_views<-}
\alias{concept_views<-,taxlist,integer-method}
\alias{concept_views<-,taxlist,data.frame-method}

\title{Management of concept views in 'taxlist' objects.}
\usage{
add_view(taxlist, View, ...)
\S4method{concept_views}{taxlist}(taxlist, ...)
concept_views(taxlist) <- value
}
\arguments{
\item{taxlist}{Object of class \code{data.frame} or \code{taxlist}.}
\item{View}{Integer vector indicating the number for the view to be added.}
\item{value}{Object of class \code{integer} or \code{data.frame} containing
	information about concept views implemented.}
\item{...}{Further arguments passed to or from other methods.}
}
\description{
The function \code{concept_views} retrieves the slot \code{conceptViews} from
objects of the class \code{\link[=taxlist-class]{taxlist}}.

Two replacement methods are implemented.
If \code{value} is an integer vector, it is assumed to indicate the respective
view for each concept included in the slot \code{taxonRelations}.
If \code{value} is a data frame, then it will be used to replace the slot
\code{conceptViews}.

In \code{add_view}, \code{...} represent additional variables to be inserted as
columns in slot \code{conceptViews}.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\examples{
library(taxlist)
data(Easplist)

## Subset for the genus Euclea and display of slot 'taxonRelations'
Euclea <- subset(Easplist, Traits=GENUS == "Euclea")
summary(Euclea, "all")
concept_views(Euclea)
}
\keyword{methods}
