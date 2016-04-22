\name{taxlist-class}
\docType{class}
\alias{taxlist-class}
\alias{taxlist}

\title{Class \dQuote{taxlist}.}
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
  }
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\examples{
showClass("taxlist")
}
\keyword{classes}
