
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

  # if the first element is a quoted identifier, split it away, else name <- NA
  split_name <- function(str) {
    has_name <- get_first(str, "\\s") != unquote_ident(get_first(str, "\\s"))

    nme <- get_first(str, "\\s")
    nme[!has_name] <- NA

    str <- mapply(sub, pattern = paste0(nme, " "), x = str,
                  MoreArgs = list(replacement = "", fixed = TRUE))
    str[!has_name] <- str[!has_name]

    list(name = unquote_ident(nme),
         rest = str)
  }

  # split into type, everything in parentheses, rest
  split_type <- function(str) {
    # extract everything from first "(" to last ")"
    len <- substr(str, attr(regexpr("^(.+?)\\(", str), "match.length"),
                  attr(regexpr("^(.+)\\)", str), "match.length"))
    len[len == ""] <- " "
    names(len) <- NULL

    typ <- get_first(str, len, fixed = TRUE)
    len[len == " "] <- ""

    str <- mapply(sub, pattern = paste0(typ, len, " "), x = str,
                  MoreArgs = list(replacement = "", fixed = TRUE))
    len[len == ""] <- NA

    list(type = toupper(typ),
         length = sub("^\\(", "", sub("\\)$", "", len)),
         rest = str)
  }

  # coerce to integer or split ENUM/SET into levels
  parse_length <- function(str) {
    str <- tryCatch(as.integer(str), warning = function(w) {
      if (grepl("^NAs introduced by coercion$", conditionMessage(w)))
        str
      else
        warning(w)
    })

    to_parse <- !is.integer(str) & !is.null(str) & !is.na(str)
    if (length(str[to_parse]) > 0) {
      str[to_parse] <- lapply(strsplit(str[to_parse], "',\\s*'"), unquote_str,
                              USE.NAMES = FALSE)
      if (all(sapply(str, length) == 1L)) str <- unlist(str)
    }

    str
  }

  # replace multiple whitespace with single and remove leading whitespace
  x <- sub("^\\s", "", gsub("\\s+", " ", x))

  field <- split_name(x)

  type <- split_type(field$rest)

  tibble::tibble(Field = field$name,
                 Type = type$type,
                 Length = parse_length(type$length),
                 Unsigned = grepl("UNSIGNED", x, ignore.case = TRUE),
                 Null = !grepl("NOT NULL", x, ignore.case = TRUE))
}
