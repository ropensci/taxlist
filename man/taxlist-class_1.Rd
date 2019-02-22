\name{count_taxa}
\docType{methods}
\alias{count_taxa}
\alias{count_taxa,character-method}
\alias{count_taxa,factor-method}
\alias{count_taxa,taxlist-method}

\title{Count taxa within a taxlist object}
\description{
Counting number of taxa within \code{\linkS4class{taxlist}} objects or character
vectors containing taxon names.
}
\usage{
\S4method{count_taxa}{character}(object, rm.na=TRUE, ...)

\S4method{count_taxa}{factor}(object, rm.na=TRUE, ...)

\S4method{count_taxa}{taxlist}(object, level, ...)
}
\arguments{
\item{object}{An object of class \code{\linkS4class{taxlist}}.}
\item{rm.na}{Logical value, whether NAs have to be removed from the input
	vector or not.}
\item{level}{Character value indicating the taxonomic rank of counted taxa.}
\item{...}{further arguments passed among methods.}
}
\details{
This function is written by convenience in order to reduce code for counting
taxa within \code{\linkS4class{taxlist}} objects and it is just a wrapper of
\code{\link{length}}.
}
\value{
An integer with the number of taxa.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\examples{
library(taxlist)

## factor method
count_taxa(iris$Species)

## taxlist method
count_taxa(Easplist)
count_ta