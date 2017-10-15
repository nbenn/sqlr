
is_int <- function(x,
                   n_elem = gte(1L),
                   allow_na = FALSE,
                   allow_null = FALSE) {

  if (allow_null & is.null(x)) return(TRUE)
  if (!is.vector(x)) return(FALSE)

  n_elem <- n_elem(length(x))

  if (!is.integer(x)) return(FALSE)
  if (!allow_na & any(is.na(x))) return(FALSE)
  if (!n_elem) return(FALSE)
  TRUE
}

is_lgl <- function(x,
                   n_elem = gte(1L),
                   allow_na = FALSE,
                   allow_null = FALSE) {

  if (allow_null & is.null(x)) return(TRUE)
  if (!is.vector(x)) return(FALSE)

  n_elem <- n_elem(length(x))

  if (!is.logical(x)) return(FALSE)
  if (!allow_na & any(is.na(x))) return(FALSE)
  if (!n_elem) return(FALSE)
  TRUE
}

is_chr <- function(x,
                   n_char = gte(1L),
                   n_elem = gte(1L),
                   allow_na = FALSE,
                   allow_null = FALSE) {

  if (allow_null & is.null(x)) return(TRUE)
  if (!is.vector(x)) return(FALSE)

  n_char <- n_char(nchar(x[!is.na(x)]))
  n_elem <- n_elem(length(x))

  if (!is.character(x)) return(FALSE)
  if (!allow_na & any(is.na(x))) return(FALSE)
  if (!n_elem) return(FALSE)
  if (length(nchar) > 0 && !all(n_char[!is.na(n_char)])) return(FALSE)
  TRUE
}

lt  <- function(x) function(y) y <  x
lte <- function(x) function(y) y <= x
gt  <- function(x) function(y) y >  x
gte <- function(x) function(y) y >= x
eq  <- function(x) function(y) y == x
