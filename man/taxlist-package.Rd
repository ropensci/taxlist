\name{taxlist-package}
\docType{package}
\alias{taxlist-package}

\title{Handling Taxonomic Lists}
\description{
The class \code{\linkS4class{taxlist}} is defined in this package using the S4
language.
The main task of \code{taxlist} objects is to keep the taxonomic coherence of
information included in taxonomic lists and to implement functions (methods) for
a proper data handling.
Objects of class \code{\linkS4class{taxlist}} can be included in further
objects, for instance in biodiversity records as done in the package
\href{https://cran.r-project.org/package=vegtable}{vegtable}.
More detailed examples on the applications of this package will be discussed by
\href{https://doi.org/10.1111/j.1654-1103.2010.01209.x}{Alvarez & Luebert (2018)}.
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
