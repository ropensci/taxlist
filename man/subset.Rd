\name{subset}
\docType{methods}
\alias{subset}
\alias{subset,taxlist-method}

\title{Subset Method for taxlist Objects.}
\description{
A subset of \code{\linkS4class{taxlist}} objects will be done either
by \code{taxonNames}, \code{taxonRelations} or \code{taxonTraits} slots.
}
\usage{
\S4method{subset}{taxlist}(x, subset, slot="names", ...)
}
\arguments{
\item{x}{object of class \code{\link[=taxlist-class]{taxlist}}.}
\item{subset}{logical vector or logical operation to apply as subset.}
\item{slot}{character value indicating the slot to be used for the subset.}
\item{...}{further arguments to be passed to or from other methods.}
}
\details{
This method is applied to objects of class \code{\linkS4class{taxlist}}
in order to generate subsets of such objects.
The argument \code{subset} will be applied to the slot specified in argument
\code{slot}.
In the later you can use either the textual name of the slot or an alias. 

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

## Loading GermanSL 1.3 (http://www.botanik.uni-greifswald.de/2305.html)
GermanSL <- tv2taxlist("GermanSL_1.3", file.path(path.package("taxlist"),
	"tv_data"))

## Subset of annual plants
GermanSL <- subset(GermanSL, grepl("T", LF_LEB, fixed=TRUE), slot="traits")
summary(GermanSL)

summary(as.factor(GermanSL$LF_LEB))
}
\keyword{methods}
