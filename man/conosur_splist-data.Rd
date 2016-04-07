\name{conosur_splist-data}
\docType{data}
\alias{conosur_splist-data}
\alias{conosur_splist}

\title{Species List for Argentina and Chile}
\description{
This list contains accepted names and synonyms downloaded from the
\dQuote{Flora del Conosur}, which is available at
\url{http://www2.darwin.edu.ar/Proyectos/FloraArgentina/fa.htm}.

The list was loaded using the package \pkg{conosur} and represent the state of
18-12-2015.}
\usage{
data(conosur_splist)
}
\format{
A data frame containing information for 64,182 taxon names (species and
sub-specific taxa) in following variables (columns):
\describe{
	\item{TaxonUsageID}{Code for the name.}
	\item{TaxonConceptID}{Code for the taxon.}
	\item{TaxonName}{Full name of taxon.}
	\item{AuthorName}{Author of the combination (taxon name).}
	\item{genus}{genus name.}
	\item{species}{species name.}
	\item{subspecies}{sub-species name.}
	\item{variety}{variety name.}
	\item{form}{form name.}
	\item{Link}{link to the name's page in \dQuote{Flora del Conosur}.}
}
}
\source{
\url{http://www2.darwin.edu.ar/Proyectos/FloraArgentina/fa.htm}
}
\keyword{datasets}
