\name{taxonRelations}
\docType{methods}
\alias{taxonRelations}
\alias{taxonRelations,data.frame-method}
\alias{taxonRelations,taxlist-method}
\alias{taxonRelations<-}
\alias{taxonRelations<-,taxlist,data.frame-method}

\title{Retrieve or Assign Slot \dQuote{taxonRelations} to a \dQuote{taxlist}
Object}
\usage{
\S4method{taxonRelations}{data.frame}(taxlist, ...)
\S4method{taxonRelations}{taxlist}(taxlist, ...)
taxonRelations(taxlist) <- value
}
\arguments{
	\item{taxlist}{Object of class \code{data.frame} or \code{taxlist}.}
	\item{value}{Object of class \code{data.frame} to set as slot
		\code{taxonRelations}.}
	\item{...}{Further arguments passed to or from other methods.}
}
\description{
The function \code{taxonRelations} retrieves or assigns the slot
\code{taxonRelations} to objects of the class
\code{\link[=taxlist-class]{taxlist}}.

The \code{data.frame} method can be used to generate the \code{taxonRelations}
slot from a data frame similar to \code{\link{conosur_splist}} or from a slot
\code{taxonNames} in a \code{taxlist} object.
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\keyword{methods}
