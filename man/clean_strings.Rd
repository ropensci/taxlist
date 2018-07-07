\name{clean_strings}
\docType{methods}
\alias{clean_strings}
\alias{clean_strings,character-method}
\alias{clean_strings,factor-method}
\alias{clean_strings,data.frame-method}

\title{Cleaning Character Strings}
\description{
Multiple, leading and trailing white spaces as well as wrong encodings may cause
serious problems in information dealing with taxonomic names.
The function \code{'clean_strings'} get rid of them.
}
\usage{
\S4method{clean_strings}{character}(x, from="utf8", to="utf8")

\S4method{clean_strings}{factor}(x, from="utf8", to="utf8")

\S4method{clean_strings}{data.frame}(x, from="utf8", to="utf8")
}
\arguments{
\item{x}{Object to be cleaned.}
\item{from,to}{Arguments passed to \code{\link{iconv}}.}
}
\details{
This function automatically deletes leading, trailing and multiple white
spaces, either in strings (method \code{'character'}), levels (method
\code{'factor'}) or in single columns (method \code{'data.frame'}).
}
\value{
The same as input \code{'x'}.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\examples{
library(taxlist)
clean_strings(" Cyperus    papyrus L.     ")
}
