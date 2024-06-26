#' @name replace_x
#'
#' @title Data manipulation.
#'
#' @description
#' This is a series of functions designed for a fast coding of replacements
#' both, as internal functions and in workflows dealing with information stored
#' in vectors.
#' Such functions are especially useful when handling with functional traits
#' stored in [taxlist-class] objects.
#'
#' `replace_x()` is used to exchange values in vectors.
#' `replace_idx()` changes values in vectors by matching indices or conditions.
#' The function `replace_na()` works in the same way as `replace_idx()` but will
#' only insert values in empty elements (NAs).
#'
#' @param x A vector to be modified. In the case of `insert_rows()`, `x` is a
#'     data frame.
#' @param old A vector with values to be replaced by `replace_x()` in a vector.
#' @param new A vector containing values to be inserted, either comparing values
#'     or using indices.
#' @param idx1,idx2 Indices applied for value replacements to match `x` with
#'     `new`, respectively. If `idx2` is not provided, it will be assumed as
#'     equivalent to `idx1`.
#'
#' @return A vector or data frame with the modified values.
#'
#' @author Miguel Alvarez.
#'
#' @example examples/replace_x.R
#'
#' @rdname replace_x
#'
#' @export
replace_x <- function(x, old, new) {
  if (length(old) != length(new)) {
    stop("Arguments 'old' and 'new' have to be of the same length.")
  }
  x[x %in% old] <- new[match(x[x %in% old], old)]
  return(x)
}

#' @rdname replace_x
#' @aliases replace_idx
#' @export
replace_idx <- function(x, idx1 = x, idx2 = idx1, new) {
  if (length(x) != length(idx1)) {
    stop("Arguments 'x' and 'idx1' have to be of the same length.")
  }
  if (length(idx2) != length(new)) {
    stop("Arguments 'idx2' and 'new' have to be of the same length.")
  }
  x[idx1 %in% idx2] <- new[match(idx1[idx1 %in% idx2], idx2)]
  return(x)
}

#' @rdname replace_x
#' @aliases replace_na
#' @export
replace_na <- function(x, idx1, idx2 = idx1, new) {
  if (length(x) != length(idx1)) {
    stop("Arguments 'x' and 'idx1' have to be of the same length.")
  }
  if (length(idx2) != length(new)) {
    stop("Arguments 'idx2' and 'new' have to be of the same length.")
  }
  x[idx1 %in% idx2 & is.na(x)] <- new[match(
    idx1[idx1 %in% idx2 & is.na(x)],
    idx2
  )]
  return(x)
}
