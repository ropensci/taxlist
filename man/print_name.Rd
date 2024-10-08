% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_name.R
\name{print_name}
\alias{print_name}
\alias{print_name.character}
\alias{print_name,character-method}
\alias{print_name.taxlist}
\alias{print_name,taxlist-method}
\title{Format usage names for publications}
\usage{
print_name(object, ...)

\method{print_name}{character}(
  object,
  second_mention = FALSE,
  style = "markdown",
  isolate = c("var.", "ssp.", "subsp.", "f.", "fma."),
  trim = c("spp.", "sp.", "species"),
  italics = TRUE,
  collapse,
  ...
)

\method{print_name}{taxlist}(
  object,
  id,
  concept = TRUE,
  include_author = TRUE,
  secundum,
  style = "markdown",
  italics = TRUE,
  collapse,
  ...
)
}
\arguments{
\item{object}{An object of class \linkS4class{taxlist}.}

\item{...}{Further arguments passed among methods.}

\item{second_mention}{Logical value, whether the genus name should be
abbreviated or not.}

\item{style}{Character value indicating the alternative format for italics.
The available options are \code{"markdown"} (called within Rmarkdown
documents), \code{"html"} (for documents rendered into html files),
\code{"expression"} (used for labels in graphics), and \code{"knitr"} (format in
LaTeX code).}

\item{isolate}{A character vector with words (usually abbreviations)
appearing in the middle of scientific names, which are not formatted in
italics.}

\item{trim}{A character vectors with words appearing at the end of scientific
names that are not formatted in italics, either.}

\item{italics}{A logical value indicating whether the names should be
italized or not.}

\item{collapse}{A character value or vector used to collapse the names and
passed to \code{\link[=paste0]{paste0()}}. If its lenght is 2, the second value will be used
to connect the two last names. Note that collapse is not yet implemented
for \code{style = "expression"}.}

\item{id}{Integer containing either a concept or a name ID.}

\item{concept}{Logical value, whether \code{id} corresponds to a concept ID
or a taxon usage name ID.}

\item{include_author}{Logical value, whether authors of the name should be
mentioned or not.}

\item{secundum}{Character value indicating the column in slot \code{taxonViews}
that will be mentioned as \emph{secundum} (according to).}
}
\value{
A character value including format to italic font.
}
\description{
When writing on bio-diversity, usage names could be automatically inserted in
documents including the typical italic format for different elements of a
scientific name.
The function \code{print_name} can be applied either in markdown documents or
for graphics.

In \strong{Rmarkdown} documents use \code{*Cyperus papyrus* L.} for
inserting a formatted a species name.
}
\examples{
## Example subspecies
summary(Easplist, 363, secundum = "secundum")

## Empty plot
plot(x = NA, xlim = c(0, 5), ylim = c(7, 1), bty = "n", xaxt = "n", xlab = "",
  ylab = "options")

## Accepted name with author
text(x = 0, y = 1, labels = print_name(Easplist, 363, style = "expression"),
  pos = 4)

## Including taxon view
text(x = 0, y = 2, labels = print_name(Easplist, 363, style = "expression",
  secundum = "secundum"), pos = 4, cex = 0.7)

## Second mention in text
text(x = 0, y = 3, labels = print_name(Easplist, 363, style = "expression",
  second_mention = TRUE), pos = 4)

## Using synonym
text(x = 0, y = 4, labels = print_name(Easplist, 50037, style = "expression",
  concept = FALSE), pos = 4)

## Markdown style
text(0, 5, labels = print_name(Easplist, 363, style = "markdown"), pos = 4)

## HTML style
text(0, 6, labels = print_name(Easplist, 363, style = "html"), pos = 4,
    cex = 0.7)

## LaTeX style for knitr
text(x = 0, y = 7, labels = print_name(Easplist, 363, style = "knitr"), pos = 4,
    cex = 0.7)
}
\seealso{
\code{\link[ape:mixedFontLabel]{ape::mixedFontLabel()}}.
}
