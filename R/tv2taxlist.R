#' @name tv2taxlist
#'
#' @title Import species lists from Turboveg databases
#'
#' @description
#' Importing species lists from
#' [Turboveg 2](https://www.synbiosys.alterra.nl/turboveg/) databases into a
#' [taxlist-class] object.
#'
#' Internally the funcions [read.dbf()] and [df2taxlist()] are called.
#'
#' @param taxlist Character value indicating the name of a species list in
#'     Turboveg.
#' @param tv_home Character value indicating the path to the main Turboveg
#'     folder. By default the function [tv.home()] from [vegdata-package] is
#'     called.
#' @param ... Further arguments passed to [df2taxlist()].
#'
#' @return A [taxlist-class] object.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @seealso [df2taxlist()]
#'
#' @example examples/tv2taxlist.R
#'
#' @export
tv2taxlist <- function(taxlist, tv_home = tv.home(), ...) {
  tv_home <- file.path(tv_home, "species", taxlist)
  species <- read.dbf(file.path(tv_home, "species.dbf"), as.is = TRUE)
  names(species) <- replace_x(names(species),
    old = c("SPECIES_NR", "ABBREVIAT", "AUTHOR", "VALID_NR"),
    new = c("TaxonUsageID", "TaxonName", "AuthorName", "TaxonConceptID")
  )
  species$AcceptedName <- !species$SYNONYM
  if (any(grepl("ecodbase.dbf", list.files(tv_home), ignore.case = TRUE))) {
    ecodbase <- read.dbf(file.path(tv_home, "ecodbase.dbf"), as.is = TRUE)
    names(ecodbase) <- replace_x(
      names(ecodbase),
      old = "SPECIES_NR",
      new = "TaxonConceptID"
    )
    species <- df2taxlist(species, taxonTraits = ecodbase, ...)
  } else {
    species <- df2taxlist(species, ...)
  }
  return(species)
}
