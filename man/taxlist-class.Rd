\name{taxlist-class}
\docType{class}
\alias{taxlist-class}
\alias{taxlist}

\title{Class taxlist.}
\description{
Class for species (taxon) lists including synonyms.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("taxlist")}.
}
\section{Slots}{
Objects of this class are composed of three slots, every one corresponding to
a data frame:
\describe{
\item{\code{taxonNames}:}{Alternative names (accepted and synonym ones) for
	every concept.}
\item{\code{taxonRelations}:}{Relations between concepts, accepted and first
	names.}
\item{\code{taxonTraits}:}{Traits of every concept (taxon).}
\item{\code{taxonViews}:}{View used to determine the respective concepts
	(relations between names).}
}
}
\details{
An extended description may be inserted here!!!
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\references{
References may be inserted in attention to the description.
}
\examples{
showClass("taxlist")
}
\keyword{classes}
