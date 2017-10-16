
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
#' @param extra_test Used by other functions, such as \code{is_int} and
#' \code{is_chr}, this injects a further test, that if it evaluates to FALSE,
#' \code{is_vec} will return FALSE. Only a quosure is accepted and it will
#' be evaluated in the context of this functions and not where it was
#' originally called.
#' 
#' @return A single logical, indicating whether all conditions are met.
#' 
#' @export
#' 
is_vec <- function(x,
                   type = c(NA, "int", "num", "lgl", "chr"),
                   n_elem = gte(1L),
                   names = FALSE,
                   allow_na = FALSE,
                   allow_null = FALSE,
                   extra_test = NULL) {

  if (!is.null(extra_test)) stopifnot(rlang::is_quosure(extra_test))

  if (is.logical(names) & length(names) == 1) {
    check_names <- names
    if (check_names)
      names <- names(x)
    else
      names <- NULL
  } else if (is.character(names))
    check_names <- TRUE
  else stop("names is expected to either be logical or character")

  type <- switch(match.arg(type),
                 int = function(x) is.integer(x) | bit64::is.integer64(x),
                 num = is.numeric,
                 lgl = is.logical,
                 chr = is.character)

  !(
    (is.null(x) & !allow_null) ||
    !is.null(x) && (
      (!bit64::is.vector.integer64(x)) ||
      (!is.null(type) && !type(x)) ||
      (check_names & (is.null(names(x)) || !setequal(names(x), names))) ||
      (!allow_na & any(is.na(x))) ||
      (!is.null(n_elem) && !n_elem(length(x))) ||
      (!is.null(extra_test) && !rlang::eval_tidy(rlang::UQE(extra_test)))
    )
  )
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
is_int <- function(..., strict = TRUE) {
  is_vec(..., type = if (strict) "int" else "num",
         extra_test = if (strict)
                        NULL
                      else
                        rlang::quo(all(floor(x) == ceiling(x), na.rm = TRUE))
         )
}

#' @param n_char Behaves analogously to \code{n_elem}, with \code{nchar(x)} on
#' the lhs.
#' 
#' @rdname is_vec
#' 
#' @export
#' 
is_chr <- function(..., n_char = gte(1L)) {
  is_vec(..., type = "chr",
         extra_test = if (is.null(n_char))
                        NULL
                      else
                        rlang::quo(all(rlang::UQ(n_char)(
                          nchar(x[!is.na(x)]))))
         )
}

#' @param b The rhs of the comparison.
#' 
#' @rdname is_vec
#' 
#' @export
#' 
lt  <- function(b) function(a) a <  b

#' @rdname is_vec
#' 
#' @export
#' 
lte <- function(b) function(a) a <= b

#' @rdname is_vec
#' 
#' @export
#' 
gt  <- function(b) function(a) a >  b

#' @rdname is_vec
#' 
#' @export
#' 
gte <- function(b) function(a) a >= b

#' @rdname is_vec
#' 
#' @export
#' 
eq  <- function(b) function(a) a == b
