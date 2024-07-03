#' @name id_generator
#'
#' @title Generate Identifiers
#'
#' @description
#' Creating identifiers for new elements in a database.
#'
#' The function `id_solver()` wil compare to set of identifiers and modify the
#' second to avoid duplicated IDs.
#'
#' @param len Numeric value indicating the length of the retrieved vector with
#'     identifiers.
#' @param minvalue Numeric value indicating the minimum value in the vector of
#'     identifiers. Used only for `'mode = "numeric"'`.
#' @param nchar Numeric value indicating the number of characters included in
#'     the retrieved identifiers. Used only for `'mode = "character"'`.
#' @param mode Character value indicating the type of identifier created, which
#'     is either numeric (the default) or charcter.
#' @param ... Further parameters passed to [stringi::stri_rand_strings()], actually to
#'     the argument `'pattern'`.
#' @param insert A vector (either numeric or character) containing IDs of
#'     elements that will be inserted in a database.
#' @param to A vector (either numeric or character) containing IDs of elements
#'     thar already exist in target database.
#' @param suffix A character vector indicating the mode used for the suffix.
#'     Only 'numeric' or 'character' and partial matchings are accepted here.
#'     This argument is only used for character IDs. If
#'     `'suffix = "character"'`, a letter of the alphabet (vector `'letters'`)
#'     will be appended to duplicated IDs.
#' @param sep A character value used as separator between original character
#'     ID and the appended suffix.
#'
#' @return
#' A vector with IDs created by `id_generator()`, either as numeric or
#' character.
#' In the case of `id_solver()`, a vector, which is either identical to
#' `'insert'` (if no conflicts) or a vector witht he same properties but with
#' resolved IDs.
#'
#' @example examples/id_generator.R
#'
#' @rdname id_generator
#'
#' @export
id_generator <- function(
    len, minvalue = 1, nchar = 10,
    mode = c("numeric", "character"), ...) {
  mode <- pmatch(mode[1], c("numeric", "character"))
  if (!mode %in% c(1:2)) {
    stop("Invalid value in 'mode'.")
  }
  if (mode == 1) {
    ID <- c(minvalue:(minvalue + len - 1))
  }
  if (mode == 2) {
    ID <- stri_rand_strings(n = len, length = nchar, ...)
  }
  return(ID)
}

#' @rdname id_generator
#' @aliases id_solver
#' @export
id_solver <- function(
    insert, to, suffix = c("numeric", "character"),
    sep = "") {
  if (any(duplicated(to))) {
    stop("Duplicated values are not allowed in 'to'.")
  }
  if (any(duplicated(insert))) {
    stop("Duplicated values are not allowed in 'insert'.")
  }
  if (is(to, "numeric")) {
    if (!is(insert, "numeric")) {
      stop("Argument 'to' has to be a numeric vector.")
    }
    idx <- insert %in% to
    insert[idx] <- id_generator(
      len = sum(idx),
      minvalue = max(c(insert, to)) + 1
    )
  }
  if (is(to, "character")) {
    if (!is(insert, "character")) {
      stop("Argument 'to' has to be a character vector.")
    }
    suffix <- pmatch(suffix[1], c("numeric", "character"))
    if (!suffix %in% c(1:2)) {
      stop("Invalid value in 'suffix'.")
    }
    if (any(insert %in% to)) {
      new_insert <- insert
      i <- 1
      idx <- insert %in% to
      repeat {
        if (suffix == 1) {
          sx <- paste0(sep, i, collapse = "")
        }
        if (suffix == 2) {
          sx <- paste0(sep, letters[i], collapse = "")
        }
        new_insert[idx] <- paste0(insert[idx], sx)
        idx2 <- rep(FALSE, length(idx))
        idx2[idx] <- new_insert[idx] %in% c(to, insert[!idx])
        idx <- idx2
        i <- i + 1
        if (sum(idx) > 0) next else break
      }
      insert <- new_insert
    }
  }
  return(insert)
}
