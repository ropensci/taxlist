#' Add a suffix when strings are identical
#'
#' An integer will be added as suffix in `x` if an identical value is in `y`.
#' If a value with suffix is already in `y`, the function will search for the
#' highest suffix to avoid duplicated values.
#'
#' @param x (`character` of length 1) The name to be compared.
#' @param y (`character`) Existing names, with or without suffixes.
#' @param sep (`character` of length 1) Symbol to be placed between the original
#'     name and the suffix.
#'
#' @return A `character` value, either `x` or `x` with suffix if already in `y`.
#'
#' @keywords internal
add_suffix <- function(x, y, sep = "_") {
  if (x %in% y) {
    i <- 0
    repeat{
      i <- i + 1
      if (paste(x, i, sep = sep) %in% y) next else break
    }
    x <- paste(x, i, sep = sep)
  }
  return(x)
}

#' Filling missed columns with NAs
#'
#' If columns of `y` are missed in `x`, the later gets these columns filled with
#' `NA` values.
#'
#' @param x (`data.frame`) The data frame to be compared.
#' @param y (`data.frame`) The data frame used as reference.
#'
#' @return A `data.frame`.
#'
#' @keywords internal
add_nacolumn <- function(x, y) {
  for (i in y[!y %in% colnames(x)]) {
    x[, i] <- NA
  }
  return(x)
}

#' Inserting new rows and columns by merging two data frames
#'
#' Two data frames sharing partially columns will be merged including the sum
#' of all variables.
#'
#' @param x,y (`data.frame`) The data frames to be merged.
#'
#' @keywords internal
two2one_df <- function(x, y) {
  x <- add_nacolumn(x, colnames(y))
  y <- add_nacolumn(y, colnames(x))
  x <- do.call(rbind, list(x, y[, colnames(x)]))
  return(x)
}

#' Distribute parental taxa across taxon concepts
#'
#' A data frame including chains of parental taxa for each taxon concept.
#'
#' @param object (`taxlist`) A taxlist object including taxonomic ranks and
#'     parent-child relationships.
#'
#' @keywords internal
arrange_taxa <- function(object) {
  # first entry with concepts at level
  if (all(is.na(object@taxonRelations$Level))) {
    stop("Input object without taxonomic ranks.")
  }
  if (all(is.na(object@taxonRelations$Parent))) {
    stop("Input object without any parent-child relationships.")
  }
  TAX <- object@taxonRelations[, "TaxonConceptID", drop = FALSE]
  for (i in levels(object@taxonRelations$Level)) {
    ID <- object@taxonRelations[
      paste(object@taxonRelations$Level) == i,
      "TaxonConceptID"
    ]
    TAX[, i] <- ID[match(object@taxonRelations$TaxonConceptID, ID)]
  }
  # second entry parents
  for (i in levels(object@taxonRelations$Level)[
    -length(levels(object@taxonRelations$Level))
  ]) {
    if (!all(is.na(TAX[, i]))) {
      TAX <- split(TAX, is.na(TAX[, i]))
      ID <- TAX[["FALSE"]][, i]
      PAR <- object@taxonRelations[
        match(
          ID,
          object@taxonRelations$TaxonConceptID
        ),
        "Parent"
      ]
      LEV <- paste(object@taxonRelations[
        match(
          PAR,
          object@taxonRelations$TaxonConceptID
        ),
        "Level"
      ])
      LEV[LEV == "NA"] <- NA
      for (j in unique(LEV[!is.na(LEV)])) {
        ID_2 <- ID[LEV == j]
        PAR_2 <- PAR[LEV == j]
        TAX[["FALSE"]][, j] <- PAR_2[match(
          TAX[["FALSE"]][, i],
          ID_2
        )]
      }
      TAX <- do.call(rbind, TAX)
    }
  }
  rownames(TAX) <- NULL
  return(TAX)
}

#' Guess Turboveg 2 installation path.
#'
#' Reads and set invisibly `option("tv_home")`.
#'
#' @param recheck (logical) reset even if option('tv_home') is already set.
#'
#' @return Reads and sets invisbly option('tv_home').
#'
#' @author Florian Jansen
#'
#' @keywords internal
# Code extracted from vegdata v. 0.9
# (https://cran.r-project.org/src/contrib/Archive/vegdata/)
tv.home <- function(recheck = FALSE) {
  if (is.null(getOption("tv_home")) | recheck) {
    if (.Platform$OS.type == "unix") {
      if ("Turbowin" %in% list.dirs(path = paste(Sys.getenv("HOME"),
        "/.wine/drive_c",
        sep = ""
      ), full.names = FALSE, recursive = FALSE)) {
        tv_home <- file.path(Sys.getenv("HOME"), ".wine/drive_c/Turbowin")
      } else {
        tv_home <- NA
      }
    }

    if (.Platform$OS.type == "windows") {
      if (file.access("O:/Turbowin/Popup/tvscale.dbf") == 0) {
        tv_home <- "O:/Turbowin"
      } else if (file.access("C:/Turbowin/Popup/tvscale.dbf") == 0) {
        tv_home <- "C:/Turbowin"
      } else if (file.access("C:/Programs/Turbowin/Popup/tvscale.dbf") == 0) {
        tv_home <- "C:/Programs/Turbowin"
      } else if (file.access("C:/Programme/Turbowin/Popup/tvscale.dbf") == 0) {
        tv_home <- "C:/Programme/Turbowin"
      } else if (file.access("D:/Programme/Turbowin/Popup/tvscale.dbf") == 0) {
        tv_home <- "D:/Programme/Turbowin"
      } else {
        tv_home <- NA
      }
    }
    if (is.na(tv_home)) {
      message("\nNo Turbowin installation path found. \n")
      if (interactive()) {
        ANSWER <- readline("Should I use \n 1) the vegdata package path (recommended),  or \n 2) a temporary folder? ")
        tv_home <- switch(substr(ANSWER, 1, 1),
          "1" = file.path(path.package("vegdata"), "tvdata"),
          tempdir()
        )
      } else {
        tv_home <- tempdir()
      }
      options(tv_home = tv_home)
      if (!file.exists(file.path(tv_home, "tvdata", "Popup", "tvscale.dbf"))) {
        for (d in c("Popup", "Data", "Species")) {
          dir.create(file.path(tv_home, d), showWarnings = FALSE)
          if (d == "Data") {
            wd <- getwd()
            setwd(file.path(path.package("vegdata"), "tvdata", "Data"))
            dbs <- list.dirs(, recursive = TRUE, full.names = FALSE)
            for (l in 2:length(dbs)) {
              dir.create(file.path(tv_home, "Data", dbs[l]),
                showWarnings = FALSE
              )
              file.copy(
                from = list.files(dbs[l],
                  recursive = TRUE,
                  full.names = TRUE, include.dirs = TRUE
                ),
                to = file.path(tv_home, "Data", dbs[l])
              )
            }
            setwd(wd)
          } # else {
          # file.copy(from = list.files(file.path(path.package("vegdata"),
          # "tvdata", d), recursive = TRUE, full.names = TRUE,
          # include.dirs = TRUE), to = file.path(tv_home, d))
          # }
        }
      }
    }
    message(
      "############################################################",
      '\nTurboveg root directory is set to "', tv_home, '"',
      "\nIf you want to change this use:",
      'options(tv_home=\"<path_to_your_Turbowin_root>\")',
      "\n############################################################"
    )
    options(tv_home = tv_home)
  } else {
    tv_home <- getOption("tv_home")
    #  message('Turboveg root directory has already been set to "',
    # tv_home,'".\n', sep='')
  }
  invisible(tv_home)
}
