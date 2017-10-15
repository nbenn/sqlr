
is_vec <- function(x,
                   type = c(NA, "int", "num", "lgl", "chr"),
                   n_elem = gte(1L),
                   allow_na = FALSE,
                   allow_null = FALSE,
                   extra_test = NULL) {

  if (!is.null(extra_test)) stopifnot(rlang::is_quosure(extra_test))

  type <- switch(match.arg(type),
                 int = is.integer,
                 num = is.numeric,
                 lgl = is.logical,
                 chr = is.character)

  browser()

  rlang::get_env(extra_test)

  if (allow_null & is.null(x))
    TRUE
  else if ( !is.vector(x) ||
           (!is.null(type) && !type(x)) ||
           (!allow_na & any(is.na(x))) ||
           (!is.null(n_elem) && !n_elem(length(x))) ||
           (!rlang::quo_is_null(extra_test) &&
             rlang::eval_tidy(extra_test, env = child_env())))
    FALSE
  else
    TRUE
}

is_lgl <- function(...) is_vec(..., type = "lgl")
is_num <- function(...) is_vec(..., type = "num")
is_int <- function(..., strict = TRUE)
  is_vec(..., type = if (strict) "int" else "num")
is_chr <- function(..., n_char = gte(1L)) {
  is_vec(..., type = "chr",
         extra_test = rlang::quo(!is.null(rlang::UQ(n_char)) &&
                                 !all(rlang::UQ(n_char)(nchar(x[!is.na(x)])))))
}

lt  <- function(x) function(y) y <  x
lte <- function(x) function(y) y <= x
gt  <- function(x) function(y) y >  x
gte <- function(x) function(y) y >= x
eq  <- function(x) function(y) y == x
