\name{change_concept}
\docType{methods}
\alias{change_cocept}
\alias{change_concept<-}
\alias{change_concept<-,taxlist-method}

\title{Move a name to a different concept.}
\description{
Replacement method for moving a name to another taxon concept in case of changes
in taxon circumscription or misplacement of synonyms.
}
\usage{
change_concept(taxlist, UsageID) <- value
}
\arguments{
\item{taxlist}{A \code{\linkS4class{taxlist}}.}
\item{UsageID}{Usage IDs to be moved from one concept to another.}
\item{value}{Destination concept ID for the moved name.}
}
\details{
Existing names will be moved from one concept to another in the case that a
concept changes its circumscription or when a synonym was assigned to a wrong
concept.
This action is not allowed for accepted names and in such cases an error message
will be retrieved by this function.
}
\value{
An object of class \code{\linkS4class{taxlist}} with modified entries.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(taxlist)
data(Easplist)

## Display two Typha species
summary(Easplist, c(50105,573))

## Move the name Typha aethiopica to concept 573 (T. latifolia)
change_concept(Easplist, 53130) <- 573
summary(Easplist, c(50105,573))
\donttest{
## Attempting to move an accepted name
change_concept(Easplist, 50105) <- 573
}
}
\keyword{methods}
