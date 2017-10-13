
is_int <- function(x,
                   n_elem = gte(1),
                   allow_na = FALSE,
                   allow_null = FALSE) {

  if (allow_null & is.null(x)) return(TRUE)
  if (!is.vector(x)) return(FALSE)

  n_elem <- add_arg(rlang::enquo(n_elem), length(x))

  if (!is.integer(x)) return(FALSE)
  if (!allow_na & any(is.na(x))) return(FALSE)
  if (!n_elem) return(FALSE)
  TRUE
}

is_lgl <- function(x,
                   n_elem = gte(1),
                   allow_na = FALSE,
                   allow_null = FALSE) {

  if (allow_null & is.null(x)) return(TRUE)
  if (!is.vector(x)) return(FALSE)

  n_elem <- add_arg(rlang::enquo(n_elem), length(x))

  if (!is.logical(x)) return(FALSE)
  if (!allow_na & any(is.na(x))) return(FALSE)
  if (!n_elem) return(FALSE)
  TRUE
}

is_chr <- function(x,
                   n_char = gte(1),
                   n_elem = gte(1),
                   allow_na = FALSE,
                   allow_null = FALSE) {

  if (allow_null & is.null(x)) return(TRUE)
  if (!is.vector(x)) return(FALSE)

  n_char <- add_arg(rlang::enquo(n_char), nchar(x[!is.na(x)]))
  n_elem <- add_arg(rlang::enquo(n_elem), length(x))

  if (!is.character(x)) return(FALSE)
  if (!allow_na & any(is.na(x))) return(FALSE)
  if (!n_elem) return(FALSE)
  if (length(nchar) > 0 && !all(n_char[!is.na(n_char)])) return(FALSE)
  TRUE
}

add_arg <- function(quo,
                    arg) {

  if (rlang::quo_is_null(quo)) NULL
  else {
    stopifnot(rlang::quo_is_lang(quo))
    quo <- rlang::eval_tidy(rlang::lang_modify(quo, a = arg))
  }
}

lt  <- `>`
lte <- `>=`
gt  <- `<`
gte <- `<=`
eq  <- `==`