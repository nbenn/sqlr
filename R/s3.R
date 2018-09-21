new_sqlr <- function(x, subclass) {
  structure(x, class = c(paste0("sqlr_", subclass), "sqlr"))
}

#' Create an SQL representation of an object
#'
#' TBD.
#'
#' @param x An object of class `sqlr`
#' @param con A connection used to determine the SQL dialect to be used
#' @param ... For extensibility.
#' @export
sqlr_render <- function(x, con, ...) UseMethod("sqlr_render")

#' @export
sqlr_render.list <- function(x, con, ...) {
  DBI::SQL(unlist(lapply(x, sqlr_render, con = con, ...)))
}

#' @export
print.sqlr <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
format.sqlr <- function(x, ...) {
  paste0(gsub("^sqlr_", "", class(x)[[1]]), "()")
}
