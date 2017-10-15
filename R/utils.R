
is_int <- function(x,
                   n_elem = gte(1L),
                   allow_na = FALSE,
                   allow_null = FALSE) {

  if (allow_null & is.null(x))
    TRUE
  else if ( !is.vector(x) ||
            !is.integer(x) ||
           (!allow_na & any(is.na(x))) ||
           (!is.null(n_elem) && !n_elem(length(x))))
    FALSE
  else
    TRUE
}

is_lgl <- function(x,
                   n_elem = gte(1L),
                   allow_na = FALSE,
                   allow_null = FALSE) {

  if (allow_null & is.null(x))
    TRUE
  else if ( !is.vector(x) ||
            !is.logical(x) ||
           (!allow_na & any(is.na(x))) ||
           (!is.null(n_elem) && !n_elem(length(x))))
    FALSE
  else
    TRUE
}

is_chr <- function(x,
                   n_char = gte(1L),
                   n_elem = gte(1L),
                   allow_na = FALSE,
                   allow_null = FALSE) {

  if (allow_null & is.null(x))
    TRUE
  else if ( !is.vector(x) ||
            !is.character(x) ||
           (!allow_na & any(is.na(x))) ||
           (!is.null(n_elem) && !n_elem(length(x))) ||
           (!is.null(n_char) && !all(n_char(nchar(x[!is.na(x)])))))
    FALSE
  else
    TRUE
}

lt  <- function(x) function(y) y <  x
lte <- function(x) function(y) y <= x
gt  <- function(x) function(y) y >  x
gte <- function(x) function(y) y >= x
eq  <- function(x) function(y) y == x
