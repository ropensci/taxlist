\name{taxonNames}
\docType{methods}
\alias{taxonNames}
\alias{taxonNames,taxlist-method}
\alias{taxonNames<-}
\alias{taxonNames<-,taxlist,data.frame-method}

\title{Retrieve or Assign Slot \dQuote{taxonNames} to a \dQuote{taxlist} Object}
\usage{
\S4method{taxonNames}{taxlist}(taxlist, ...)
taxonNames(taxlist) <- value
}
\arguments{
	\item{taxlist}{Object of class \code{taxlist}.}
	\item{value}{Object of class \code{data.frame} to set as slot
		\code{taxonNames}.}
	\item{...}{Further arguments passed to or from other methods.}
}
\description{
The function \code{taxonNames} retrieves or assigns the slot \code{taxonNames}
to objects of the class \code{\link[=taxlist-class]{taxlist}}.
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\keyword{methods}
