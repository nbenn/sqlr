
#' @title Unquote identifiers
#' 
#' @description Quoted identifiers such as produced by
#' \code{DBI::dbQuoteIdentifier(con, "foo")} are unquoted and stripped of their
#' \code{SQL} class, yielding the original character vector (here "foo").
#' 
#' @name unquote_ident
#' 
#' @param ... Arguments passed to the S3 methods
#' @param con A connection used to determine the SQL dialect to be used
#' 
#' @return Character vector.
#' 
#' @export
#' 
unquote_ident <- function(..., con = get_con()) UseMethod("unquote_ident", con)

#' @rdname unquote_ident
#' 
#' @param x Character vector to be unquoted.
#' 
#' @export
#' 
unquote_ident.MariaDBConnection <- function(x, con = get_con()) {
  as.character(sub("^`", "", sub("`$", "", gsub("``", "`", x))))
}

parse_col_def <- function(x, name = TRUE) {

  get_first <- function(...) sapply(strsplit(...), `[`, 1L)

  if (name) {
    field <- get_first(x, "\\s")
    tmp <- sub(paste0(field, " "), "", x)
  } else {
    field <- NA
    tmp <- x
  }

  parens <- substr(tmp, regexpr("\\(", tmp), regexpr("\\)", tmp))
  parens <- sub("^\\(", "", sub("\\)$", "", parens))

  tmp <- mapply(sub, pattern = parens, x = tmp,
                MoreArgs = list(replacement = ""))
  tmp <- sub("\\(\\)", "", tmp)

  tibble::tibble(Field = field, Type = get_first(tmp, "\\s"), Length = parens,
                 Unsigned = grepl("UNSIGNED", x, ignore.case = TRUE),
                 Null = !grepl("NOT NULL", x, ignore.case = TRUE))
}
