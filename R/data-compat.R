
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
unquote_ident.MariaDBConnection <- function(x, con = get_con(), ...) {
  as.character(sub("^`", "", sub("`$", "", gsub("``", "`", x))))
}


#' @title Unquote strings
#' 
#' @description Quoted strings such as produced by
#' \code{DBI::dbQuoteString(con, "foo")} are unquoted and stripped of their
#' \code{SQL} class, yielding the original character vector (here "foo").
#' 
#' @name unquote_str
#' 
#' @param ... Arguments passed to the S3 methods
#' @param con A connection used to determine the SQL dialect to be used
#' 
#' @return Character vector.
#' 
#' @export
#' 
unquote_str <- function(..., con = get_con()) UseMethod("unquote_str", con)

#' @rdname unquote_str
#' 
#' @param x Character vector to be unquoted.
#' 
#' @export
#' 
unquote_str.MariaDBConnection <- function(x, con = get_con(), ...) {
  patt <- c("\\\\'", "\\\\\"", "\\\\n", "\\\\r", "\\\\\\\\", "\\\\Z")
  repl <- c("'", "\"", "\n", "\r", "\\\\", "\x1a")
  for (i in seq_along(patt)) x <- gsub(patt[i], repl[i], x)
  as.character(sub("^'", "", sub("'$", "", x)))
}

#' @title Unquote identifiers
#' 
#' @description Quoted identifiers such as produced by
#' \code{DBI::dbQuoteIdentifier(con, "foo")} are unquoted and stripped of their
#' \code{SQL} class, yielding the original character vector (here "foo").
#' 
#' @name parse_col_def
#' 
#' @param ... Arguments passed to the S3 methods
#' @param con A connection used to determine the SQL dialect to be used
#' 
#' @return Character vector.
#' 
#' @section TODO: what about character encoding? ie if UTF chars are to be
#' written to an ascii column: catch that as well?
#' 
#' @export
#' 
parse_col_def <- function(..., con = get_con()) UseMethod("parse_col_def", con)

#' @rdname parse_col_def
#' 
#' @param x Character vector to be parsed.
#' 
#' @export
#' 
parse_col_def.MariaDBConnection <- function(x,
                                            con = get_con(),
                                            ...) {

  get_first <- function(...) sapply(strsplit(...), `[`, 1L)

  # replace multiple whitespace with single and remove leading whitespace
  x <- sub("^\\s", "", gsub("\\s+", " ", x))

  has_name <- get_first(x, "\\s") != unquote_ident(get_first(x, "\\s"))

  field <- get_first(x, "\\s")
  field[!has_name] <- NA

  tmp <- mapply(sub, pattern = paste0(field, " "), x = x,
                MoreArgs = list(replacement = ""))
  tmp[!has_name] <- x[!has_name]

  len <- substr(tmp, regexpr("\\(", tmp), regexpr("\\)", tmp))
  len <- sub("^\\(", "", sub("\\)$", "", len))
  names(len) <- NULL

  tmp <- mapply(sub, pattern = len, x = tmp,
                MoreArgs = list(replacement = ""))
  tmp <- sub("\\(\\)", "", tmp)

  len[len == ""] <- NA
  len <- tryCatch(as.integer(len), warning = function(w) {
    if (grepl("^NAs introduced by coercion$", conditionMessage(w)))
      len
    else
      warning(w)
  })

  parse <- !is.integer(len) & !is.null(len)
  len[parse] <- sapply(len[parse], strsplit, ",\\s*", USE.NAMES = FALSE)
  if (all(sapply(len, length) == 1L)) len <- unlist(len)

  tibble::tibble(Field = unquote_ident(field),
                 Type = as.character(get_first(tmp, "\\s")),
                 Length = len,
                 Unsigned = grepl("UNSIGNED", x, ignore.case = TRUE),
                 Null = !grepl("NOT NULL", x, ignore.case = TRUE))
}
