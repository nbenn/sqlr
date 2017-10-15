
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

  if (allow_null & is.null(x))
    TRUE
  else if ( !bit64::is.vector.integer64(x) ||
           (!is.null(type) && !type(x)) ||
           (check_names &
             (is.null(names(x)) || !setequal(names(x), names))) ||
           (!allow_na & any(is.na(x))) ||
           (!is.null(n_elem) && !n_elem(length(x))) ||
           (!is.null(extra_test) && !rlang::eval_tidy(rlang::UQE(extra_test))))
    FALSE
  else
    TRUE
}

is_lgl <- function(...) is_vec(..., type = "lgl")
is_num <- function(...) is_vec(..., type = "num")
is_int <- function(..., strict = TRUE)
  is_vec(..., type = if (strict) "int" else "num",
         extra_test = if (strict)
                        NULL
                      else
                        rlang::quo(all(floor(x) == ceiling(x), na.rm = TRUE))
         )
is_chr <- function(..., n_char = gte(1L)) {
  is_vec(..., type = "chr",
         extra_test = if (is.null(n_char))
                        NULL
                      else
                        rlang::quo(all(rlang::UQ(n_char)(
                          nchar(x[!is.na(x)]))))
         )
}

lt  <- function(x) function(y) y <  x
lte <- function(x) function(y) y <= x
gt  <- function(x) function(y) y >  x
gte <- function(x) function(y) y >= x
eq  <- function(x) function(y) y == x
