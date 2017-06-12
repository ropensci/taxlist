\name{subset}
\docType{methods}
\alias{subset}
\alias{subset,taxlist-method}

\title{Subset method for \sQuote{taxlist} objects.}
\description{
Subset of \code{\linkS4class{taxlist}} objects will be done applying either
logical operations or pattern matchings.
Subsets can be referred to information contained either in the slot
\code{taxonNames}, \code{taxonRelations} or \code{taxonTraits}.
}
\usage{
\S4method{subset}{taxlist}(x, subset, slot="names", ...)
}
\arguments{
\item{x}{Object of class \code{\linkS4class{taxlist}}.}
\item{subset}{Logical vector or logical operation to apply as subset.}
\item{slot}{Character value indicating the slot to be used for the subset.}
\item{...}{Further arguments to be passed to or from other methods.}
}
\details{
The argument \code{subset} will be applied to the slot specified in argument
\code{'slot'}.
This argument can be referred either by the full name of the respective slot or
an alias.

Be aware that \code{subset} won't work properly inside of function definitions.
}
\value{
An object of class \code{\linkS4class{taxlist}}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(taxlist)

data(Easplist)
Easplist <- subset(Easplist, LifeForm == "T", slot="traits")
summary(Easplist)

summary(as.factor(Easplist$LifeForm))
}
\keyword{methods}
