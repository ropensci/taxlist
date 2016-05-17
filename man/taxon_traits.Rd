\name{taxon_traits}
\docType{methods}
\alias{taxon_traits}
\alias{taxon_traits,taxlist-method}
\alias{taxon_traits<-}
\alias{taxon_traits<-,taxlist,data.frame-method}
\alias{[,taxlist-method}
\alias{[<-}
\alias{[<-,taxlist-method}
\alias{$,taxlist-method}
\alias{$<-}
\alias{$<-,taxlist-method}

\title{Retrieve or Assign Slot \dQuote{taxonTraits} to a \dQuote{taxlist}
Object.}
\description{
The function \code{taxon_traits} retrieves or assigns the slot
\code{taxonTraits} to objects of the class
\code{\link[=taxlist-class]{taxlist}}.
}
\usage{
\S4method{taxon_traits}{taxlist}(taxlist, ...)
\S4method{$}{taxlist}(x, name)
\S4method{[}{taxlist}(x, i, j, ..., drop=FALSE)

taxon_traits(taxlist) <- value
x$name <- value
x[i,j] <- value
}
\arguments{
\item{taxlist, x}{Object of class \code{taxlist}.}
\item{value}{Data frame or vector with replacement values (depending on the
	method).}
\item{...}{Further arguments passed to or from other methods.}
\item{name}{A name to access.}
\item{i,j}{Indices for access.}
\item{drop}{A logical value passed to \code{\link{Extract}}.}
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\seealso{
\code{\link[=taxlist-class]{taxlist}}.
}
\examples{
library(taxlist)
data(Easplist)

## How many genera are in the list?
length(unique(Easplist$GENUS))

## How many families?
length(unique(Easplist$FAMILY))

## Subset for the genus Euclea and display of slot 'taxonTraits'
Euclea <- subset(Easplist, Traits=GENUS == "Euclea")
summary(Euclea, "all")
taxon_traits(Euclea)

## Example from 'df2taxlist'
setwd(file.path(path.package("taxlist"), "cyperus"))
Cyperus <- read.csv("names.csv", stringsAsFactors=FALSE)
Cyperus <- df2taxlist(Cyperus, AcceptedName=!Cyperus$SYNONYM)
summary(Cyperus)

## Assigning traits to Cyperus object
taxon_traits(Cyperus) <- read.csv("traits.csv", stringsAsFactors=FALSE)
summary(Cyperus)
}
\keyword{methods}
