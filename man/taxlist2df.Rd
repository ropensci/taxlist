% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxlist2df.R
\name{taxlist2df}
\alias{taxlist2df}
\alias{taxlist2df.taxlist}
\alias{taxlist2df,taxlist-method}
\title{Coerce taxlist objects to data frames}
\usage{
taxlist2df(x, ...)

\method{taxlist2df}{taxlist}(
  x,
  include_traits = FALSE,
  include_views = FALSE,
  standard = c("taxlist", "dwc"),
  ...
)
}
\arguments{
\item{x}{A \linkS4class{taxlist} object to be coerced.}

\item{...}{Further arguments passed among methods.}

\item{include_traits}{A logical value indicating whether taxon concept
attributes have to be included in the output or not.}

\item{include_views}{A logical value indicating whether taxon views
have to be included in the output or not.}

\item{standard}{A character value indicating the standard used to name
columns in the output data frame. Per default \code{taxlist} names are used
but it can be set to \code{dwc} for renaming some columns according to
\href{https://dwc.tdwg.org/terms/#taxon}{Darwin Core}.}
}
\description{
Transform \linkS4class{taxlist} objects into data frames.
}
