\name{taxonTraits}
\docType{methods}
\alias{taxonTraits}
\alias{taxonTraits,taxlist-method}
\alias{taxonTraits<-}
\alias{taxonTraits<-,taxlist,data.frame-method}

\title{Retrieve or Assign Slot \dQuote{taxonTraits} to a \dQuote{taxlist} Object}
\usage{
\S4method{taxonTraits}{taxlist}(taxlist, ...)
taxonTraits(taxlist) <- value
}
\arguments{
	\item{taxlist}{Object of class \code{taxlist}.}
	\item{value}{Object of class \code{data.frame} to set as slot
		\code{taxonTraits}.}
	\item{...}{Further arguments passed to or from other methods.}
}
\description{
The function \code{taxonTraits} retrieves or assigns the slot \code{taxonTraits}
to objects of the class \code{\link[=taxlist-class]{taxlist}}.
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\examples{
## Add an example
}
