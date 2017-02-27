\name{subset}
\docType{methods}
\alias{subset}
\alias{subset,taxlist-method}

\title{Subset Method for taxlist Objects.}
\description{
A subset of \code{\link[=taxlist-class]{taxlist}} objects will be done either
by \code{taxonNames}, \code{taxonRelations} or \code{taxonTraits} slots.
}
\usage{
\S4method{subset}{taxlist}(x, subset, slot, ...)
}
\arguments{
\item{x}{object of class \code{\link[=taxlist-class]{taxlist}}.}
\item{subset}{logical vector or logical operation to apply as subset.}
\item{slot}{character value indicating the slot to be used for the subset.}
\item{...}{further arguments to be passed to or from other methods.}
}
\details{
This method is applied to objects of class \code{\link[=taxlist-class]{taxlist}}
in order to generate subsets of such objects.
The criteria for subsets can be applied to any of the slots included in those
objects (they can be applied simultaneously to all three slots).

In the case of subset applied to the slot \code{taxonNames}, non-matching names
belonging to the same concepts as the matching ones will be preserved in the
subset.
}
\value{
An object of class \code{\link[=taxlist-class]{taxlist}}.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\examples{
## Add example
}
\keyword{methods}
