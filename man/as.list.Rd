\name{as.list}
\docType{methods}
\alias{S4_to_list}
\alias{as.list}
\alias{as.list,taxlist-method}

\title{Coerce an S4 object to a list.}
\description{
Coercion of S4 objects to lists can be applied to explore their content avoiding
errors caused by their validation.
}
\usage{
S4_to_list(x)

as.list(x, ...)
}
\arguments{
\item{x}{An object of class \code{\linkS4class{taxlist}} or any S4 class (see
	details).}
\item{...}{further arguments passed to or from other methods.}
}
\details{
While the function \code{'S4_to_list'} can be applied to any S4 object,
\code{'as.list'} is a method specifically defined for
\code{\linkS4class{taxlist}} class.
}
\value{
An object of class \code{\link[base]{list}}, whose elements correspond to slots
from input object.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
data(Easplist)
Easplist <- as.list(Easplist)
class(Easplist)
}
