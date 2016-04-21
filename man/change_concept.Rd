\name{change_concept}
\alias{change_concept<-}
\alias{change_concept<-,taxlist-method}

\title{Move a Name to a Different Concept}
\description{
Move a misplaced name in a proper taxon concept.
}
\usage{
change_concept(taxlist, UsageID) <- value
}
\arguments{
	\item{taxlist}{An object of class \dQuote{taxlist}.}
	\item{UsageID}{Corresponding usage ID to be changed from concept.}
	\item{value}{Usage ID of the new accepted name or first name, or new concept ID
		for a name.}
}
\details{
With \code{change_concept<-} a name (usage) can be relocated in a different taxon
concept. This function check previously that the name to be moved is neither a
accepted name nor a first name for the current taxon, otherwise an error message will
be displayed.
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\examples{
library(taxlist)
data(EAsplist)

## Add an example
}
