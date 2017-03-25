\name{taxlist-class}
\docType{class}
\alias{taxlist-class}
\alias{taxlist}

\title{Class taxlist.}
\description{
Class for species (taxon) lists including synonyms.
}
\section{Slots}{
Objects of this class are composed of three slots, every one corresponding to
a data frame:
\describe{
	\item{taxonNames}{Table of names (accepted names and synonyms).}
	\item{taxonRelations}{Relations between concepts, accepted names,
		basionyms, parents and hierarchical level.}
	\item{taxonTraits}{Table of taxon traits.}
	\item{taxonViews}{References used to determine the respective concept
		circumscription.}
}
}
\section{Check of validity}{
Using the function \code{\link{validObject}} it is possible to get a check on
the consistency of information contained in \code{\linkS4class{taxlist}}
objects.
Former issues, among others, will be tested in the validity check:
\itemize{
	\item Mandatory column names in all slots.
	\item Not duplicated IDs.
	\item Not duplicated name combinations (name and author).
	\item Not orphaned records.
	\item Parents have to be of higher hierarchy than children, when this
		information is available.
}
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(taxlist)

showClass("taxlist")

## Create an empty object
Splist <- new("taxlist")
}
\keyword{classes}
