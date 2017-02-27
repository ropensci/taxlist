\name{taxlist-package}
\docType{package}
\alias{taxlist-package}

\title{Handling Taxonomic Lists.}
\description{
This package offers some functions to handle taxonomic lists, focusing on
species lists including synonyms.
For handling taxonomic lists an \emph{ad-hoc} object class is defined, namely
\code{\link[=taxlist-class]{taxlist}}.
Such lists separate names (taxon usages) from taxa (taxon concepts), following
the terminology of \bold{Jansen & Dengler (2010)}.

This package attempts to handle taxonomic data keeping the taxonomic consistency
of the information included in the taxonomic lists but at the same time
providing some flexibility for linking such lists with several data sets based
on diversity records.

The applications of this packages are discussed by \bold{Alvarez & Luebert
(in preparation)}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\references{
\bold{Jansen F, Dengler J (2010).} Plant names in vegetation databases -- a
neglected source of bias. \emph{Journal of Vegetation Science} 21: 1179--1186.

\bold{Alvarez M, Luebert F (in preparation).} Applying object-oriented models to
taxonomic lists from Turboveg databases.
}
\examples{
\dontrun{
## Installing the last version from GitHub
library(devtools)
install_github("kamapu/taxlist")
}
}
