\name{as.list}
\docType{methods}
\alias{S4_to_list}
\alias{as.list}
\alias{as.list,taxlist-method}

\title{Coerce an S4 object to a list.}
\description{
Coercion used to explore inconsistencies in S4 objects.
}
\usage{
S4_to_list(x)

as.list(x, ...)
}
\arguments{
\item{x}{an object of class \code{\linkS4class{taxlist}}.}
\item{...}{further arguments passed to or from other methods.}
}
\details{
Details.
}
\value{
An object of class \code{list}, whose elements correspond to slots from input
object.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
data(Easplist)
Easplist <- as.list(Easplist)
class(Easplist)
}
