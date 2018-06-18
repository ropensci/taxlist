\name{as.list}
\docType{methods}
\alias{S4_to_list}
\alias{as.list}
\alias{as.list,taxlist-method}

\title{Coerce an S4 object to a list}
\description{
Coercion of S4 objects to lists can be applied to explore their content,
avoiding errors caused by their validation.
}
\usage{
\S4method{as.list}{taxlist}(x, ...)

## Default function
S4_to_list(x)
}
\arguments{
\item{x}{An object of class \code{\linkS4class{taxlist}} or any S4 class.}
\item{...}{further arguments passed to or from other methods.}
}
\details{
The function \code{S4_to_list()} transforms any S4 object to list setting slots
to elements of the list and it is running internally in the method for
\code{as.list()} and \code{\linkS4class{taxlist}} class.
}
\value{
An object of class \code{\link[base]{list}}.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\examples{
library(taxlist)
data(Easplist)
Easplist <- as.list(Easplist)
class(Easplist)
}
