\name{change_concept}
\docType{methods}
\alias{change_cocept}
\alias{change_concept<-}
\alias{change_concept<-,taxlist-method}

\title{Move a name to a different concept.}
\description{
Move a name in its respective taxon concept in case of changes in
circumscription or misplacement.
}
\usage{
change_concept(taxlist, UsageID) <- value
}
\arguments{
\item{taxlist}{an object of class \code{\link[=taxlist-class]{taxlist}}.}
\item{UsageID}{usage IDs to be moved from one concept to another.}
\item{value}{concept ID, where the usage should be moved to.}
}
\details{
With \code{change_concept} a name (usage) can be relocated in a different
taxon concept.
This function check previously that the name to be moved is not an accepted
for the current taxon, otherwise an error message will be retrieved.
}
\value{
An object of class \code{\link[=taxlist-class]{taxlist}} with replaced names.
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
