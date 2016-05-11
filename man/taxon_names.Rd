\name{taxon_names}
\docType{methods}
\alias{taxon_names}
\alias{taxon_names,taxlist-method}
\alias{taxon_names<-}
\alias{taxon_names<-,taxlist,data.frame-method}

\title{Retrieve or Assign Slot taxonNames to a taxlist Object.}
\usage{
\S4method{taxon_names}{taxlist}(taxlist, ...)

taxon_names(taxlist) <- value
}
\arguments{
	\item{taxlist}{Object of class \code{taxlist}.}
	\item{value}{Object of class \code{data.frame} to set as slot
		\code{taxonNames}.}
	\item{...}{Further arguments passed to or from other methods.}
}
\description{
The function \code{taxon_names} retrieves or assigns the slot \code{taxonNames}
to objects of the class \code{\link[=taxlist-class]{taxlist}}.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\seealso{
\code{\link[=taxlist-class]{taxlist}}.
}
\examples{
library(taxlist)
data(Easplist)

## Subset for the genus Euclea and display of slot 'taxonNames'
Euclea <- subset(Easplist, Traits=GENUS == "Euclea")
summary(Euclea)
taxon_names(Euclea)
}
\keyword{methods}
