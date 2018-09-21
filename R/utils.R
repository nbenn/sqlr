
#' @title Test if an object is a vector
#'
#' @description Intended for validating the input to functions, objects can be
#' tested if they are vectors, satisfying certain additional constraints, such
#' as type (integer, numeric, etc.), the number of elements, specified as
#' (in)equality, whether the object is named, whether it contains NA and if it
#' is NULL.
#'
#' @name is_vec
#'
#' @param x The object to be tested
#' @param type The type of vector that is expected. Possibly choices are
#' \code{NA}, \code{"int"}, \code{"num"}, \code{"lgl"}, \code{"chr"}, where NA
#' does not test for a specific type.
#' @param n_elem The number of elements that is expected. Using the functions
#' \code{lt}, \code{lte}, \code{gt}, \code{gte}, \code{eq}, an (in)equality can
#' be specified where \code{length(x)} is the lhs and the argument passed to
#' the function, the rhs.
#' @param names Either a single logical to test if the object is named or not,
#' or a character vector used to test for setequal of \code{names(x)} and
#' \code{names}.
#' @param allow_na A logical, specifying whether NA is allowed in x.
#' @param allow_null A logical, specifying whether x may be NULL.
#'
#' @return A single logical, indicating whether all conditions are met.
#'
#' @export
#'
is_vec <- function(x,
                   type = c(NA, "int", "num", "lgl", "chr", "lst"),
                   n_elem = gte(1L),
                   names = FALSE,
                   allow_na = FALSE,
                   allow_null = FALSE) {
  if (is.logical(names) & length(names) == 1) {
    check_names <- names
    if (check_names)
      names <- names(x)
    else
      names <- NULL
  } else if (is.character(names))
    check_names <- TRUE
  else stop("names is expected to either be logical or character")

  stopifnot(is.logical(allow_na), length(allow_na) == 1)
  stopifnot(is.logical(allow_null), length(allow_null) == 1)

  type <- switch(match.arg(type),
    `NA` = function(x) TRUE,
    int = function(x) is.integer(x) | bit64::is.integer64(x),
    num = is.numeric,
    lgl = is.logical,
    chr = is.character,
    lst = is.list
  )

  res <- !(
    (is.null(x) && !allow_null) ||
      !is.null(x) && (
        (!bit64::is.vector.integer64(x)) ||
          (!is.null(type) && !type(x)) ||
          (check_names && (is.null(names(x)) || !setequal(names(x), names))) ||
          (!allow_null && any(sapply(x, is.null))) ||
          (!allow_na && anyNA(x, recursive = TRUE)) ||
          (!is.null(n_elem) && !n_elem(length(x)))
      )
  )

  stopifnot(is.logical(res), length(res) == 1, !is.na(res))

  res
}

#' @param ... Arguments passed to \code{is_vec}.
#'
#' @rdname is_vec
#'
#' @export
#'
is_lgl <- function(...) is_vec(..., type = "lgl")

#' @rdname is_vec
#'
#' @export
#'
is_num <- function(...) is_vec(..., type = "num")

#' @param strict A logical switch specifying whether a whole number specified
#' as numeric (instead if integer) will be considered as an integer as well.
#'
#' @rdname is_vec
#'
#' @export
#'
is_int <- function(x, ..., strict = TRUE) {
  res <- is_vec(x, ..., type = if (strict) "int" else "num")
  if (!strict & !is.null(x))
    res && all(floor(x) == ceiling(x), na.rm = TRUE)
  else
    res
}

#' @param n_char Behaves analogously to \code{n_elem}, with \code{nchar(x)} on
#' the lhs.
#'
#' @rdname is_vec
#'
#' @export
#'
is_chr <- function(x, ..., n_char = gte(1L)) {
  res <- is_vec(x, ..., type = "chr")
  if (!is.null(n_char) & !is.null(x))
    res && all(n_char(nchar(x[!is.na(x)])))
  else
    res
}

#' @rdname is_vec
#'
#' @export
#'
is_lst <- function(...) is_vec(..., type = "lst")

#' @param b The rhs of the comparison.
#'
#' @rdname is_vec
#'
#' @export
#'
lt <- function(b) function(a) a < b

#' @rdname is_vec
#'
#' @export
#'
lte <- function(b) function(a) a <= b

#' @rdname is_vec
#'
#' @export
#'
gt <- function(b) function(a) a > b

#' @rdname is_vec
#'
#' @export
#'
gte <- function(b) function(a) a >= b

#' @rdname is_vec
#'
#' @export
#'
eq <- function(b) function(a) a == b
