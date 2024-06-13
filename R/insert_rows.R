#' @name insert_rows
#'
#' @title Insert additional rows to a data frame.
#'
#' @description
#' Adding new rows from data frame sharing some columns.
#' Data contained in `y` is assumed to be additional data and will be
#' appended.
#'
#' Columns occurring in only one of the data frames will be added to the output.
#'
#' @param x A data frame.
#' @param y A data frame including rows (and columns) to be inserted in `x`.
#' @param ... Addicional arguments passed among methods.
#'
#' @return A data frame.
#'
#' @example examples/insert_rows.R
#'
#' @exportMethod insert_rows
setGeneric(
  "insert_rows",
  function(x, y, ...) {
    standardGeneric("insert_rows")
  }
)

#' @rdname insert_rows
#' @aliases insert_rows,data.frame,data.frame-method
setMethod(
  "insert_rows", signature(x = "data.frame", y = "data.frame"),
  function(x, y, ...) {
    for (i in names(y)[!names(y) %in% names(x)]) {
      x[, i] <- NA
    }
    for (i in names(x)[!names(x) %in% names(y)]) {
      y[, i] <- NA
    }
    x <- do.call(rbind, list(x, y[names(x)]))
    return(x)
  }
)
