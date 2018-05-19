\name{taxlist-package}
\docType{package}
\alias{taxlist-package}

\title{Handling taxonomic lists.}
\description{
This package defines an S4 object class for handling taxonomic lists, namely the
\code{\linkS4class{taxlist}} class.
The main task of such objects is to keep the taxonomic coherence in the
information included in taxonomic lists and to implement functions (methods) for
the those data handling procedures that are common in the work with taxonomic
lists and its integration in biodiversity information.

More detailed examples on the applications of this package will be discussed by
\bold{Alvarez & Luebert (in preparation)}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\references{
\bold{Alvarez M, Luebert F (2018).} The taxlist package: managing plant
taxonomic lists in R. \emph{Biodiversity Data Journal} 6: e23635.
\url{https://doi.org/10.3897/bdj.6.e23635}

\bold{Jansen F, Dengler J (2010).} Plant names in vegetation databases -- a
neglected source of bias. \emph{Journal of Vegetation Science} 21: 1179--1186.
\url{https://doi.org/10.1111/j.1654-1103.2010.01209.x}
}
\examples{
\dontrun{
## Install last release from CRAN
install.packages("taxlist", dependencies=TRUE)

## Installing last version from GitHub
library(devtools)
install_github("kamapu/taxlist", build_vignette=TRUE)
}
}
