\name{taxlist-class}
\docType{class}
\alias{taxlist-class}
\alias{taxlist}

\title{Class \dQuote{taxlist}}
\description{
Class for species (taxon) lists including synonyms.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("taxlist")}.
}
\section{Slots}{
  \describe{
    \item{\code{taxonNames}:}{Object of class \code{data.frame} containing
    	alternative names (valid and synonym ones) for taxa.}
    \item{\code{taxonRelations}:}{Object of class \code{data.frame} including
    	relations between names and taxon identity.}
    \item{\code{taxonTraits}:}{Object of class \code{"data.frame"} including
    	information about traits for every taxon identity.}
  }
}
\details{
Some more details.
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\examples{
showClass("taxlist")
}
\keyword{classes}
