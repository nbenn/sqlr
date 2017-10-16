#' @export
get_col_spec <- function(x, ...) UseMethod("get_col_spec", x)

#' @export
get_col_spec.integer <- function(x, ...)
  col_spec(..., type = col_int, min = min(x), max = max(x))

#' @export
get_col_spec.integer64 <- function(x, ...)
  col_spec(..., type = col_int, min = min(x), max = max(x))

#' @export
get_col_spec.numeric <- function(x, ...) {
  col_spec(..., type = col_dbl,
           prec = if (!is.null(attr(x, "Csingle")) && attr(x, "Csingle"))
                    "single"
                  else
                    "double")
}

#' @export
get_col_spec.character <- function(x, ...) {
  lens <- na.omit(nchar(x, "bytes"))
  col_spec(..., type = col_chr, length = max(lens),
           fixed = (length(unique(lens)) == 1L & max(lens) < 256L))
}

#' @export
get_col_spec.list <- function(x, ...) get_col_spec(blob::as.blob(x), ...)

#' @export
get_col_spec.blob <- function(x, ...) {
  sizes <- sapply(x, object.size)
  col_spec(..., type = col_raw, length = max(sizes),
           fixed = (length(unique(sizes)) == 1L & max(sizes) < 256L))
}

#' @export
get_col_spec.logical <- function(x, ...)
  col_spec(..., type = col_lgl)

#' @export
get_col_spec.factor <- function(x, ...)
  col_spec(..., type = col_fct, levels = levels(x))

#' @export
get_col_spec.POSIXt <- function(x, ...)
  col_spec(..., type = col_dtm, class = "datetime")

#' @export
get_col_spec.Date <- function(x, ...)
  col_spec(..., type = col_dtm, class = "date")

#' @export
get_col_spec.difftime <- function(x, ...)
  col_spec(..., type = col_dtm, class = "time")

#' @export
get_col_spec.data.frame <- function(x, ...) {

  dots <- list(...)
  stopifnot(all(sapply(dots, length) %in% c(1, ncol(x))))
  single <- sapply(dots, length) == 1

  do.call(function(x, ...)
    mapply(function(dat, nme, ...)
      get_col_spec(dat, name = nme, ...),
      x, names(x), ..., MoreArgs = dots[single],  SIMPLIFY = FALSE),
    c(list(x = x), dots[!single]))
}
