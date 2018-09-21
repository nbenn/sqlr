
#' @title Show columns of db table(s)
#'
#' @description Display information about the columns in a given table.
#' The result may be limited using the arguments \code{like}/\code{where}
#' and more complete information can be requested using the switch \code{full}.
#' The returned tibble holds the following values for each table column (see
#' \url{https://dev.mysql.com/doc/refman/5.7/en/show-columns.html}):
#'
#' \describe{
#'   \item{Field}{The column name.}
#'   \item{Type}{The column data type.}
#'   \item{Collation}{The collation for nonbinary string columns, or NULL for
#'         other columns. This value is displayed only if you use the FULL
#'         keyword.}
#'   \item{Null}{Column nullability. The value is YES if NULL values can be
#'         stored in the column, NO if not.}
#'   \item{Key}{Whether the column is indexed: one of NA, PRI, UNI, MUL.}
#'   \item{Default}{The default value for the column. This is NULL if the
#'         column has an explicit default of NULL, or if the column definition
#'         includes no DEFAULT clause.}
#'   \item{Extra}{Any additional information that is available about a given
#'         column, such as \code{auto_increment}}
#'   \item{Privileges}{The privileges you have for the column. This value is
#'         displayed only if \code{full} is true.}
#'   \item{Comment}{Any comment included in the column definition. This value
#'         is displayed only if \code{full} is true.}
#' }
#'
#' @name show_db_cols
#'
#' @param ... Arguments passed on to further methods.
#' @param con A connection object to connect to the db.
#'
#' @return A tibble.
#'
#' @export
#'
show_db_cols <- function(..., con = get_con()) UseMethod("show_db_cols", con)

#' @param tbl The table name.
#' @param like A string, specifying a pattern to match column names against.
#' @param where An SQL expression used to filter results.
#' @param full A logical switch used to request additional information on each
#' of the returned columns.
#' @param parse Logical switch for turning the results into a SequelPro
#' inspired "Structure" view.
#'
#' @rdname show_db_cols
#'
#' @export
#'
show_db_cols.MariaDBConnection <- function(tbl,
                                           like = NULL,
                                           where = NULL,
                                           full = FALSE,
                                           parse = FALSE,
                                           con = get_con(),
                                           ...) {
  stopifnot(
    is_chr(tbl, n_elem = eq(1L)),
    is_chr(like, n_elem = eq(1L), allow_null = TRUE),
    is_lgl(full, n_elem = eq(1L)),
    is_lgl(parse, n_elem = eq(1L))
  )
  if (!is.null(where))
    stopifnot(inherits(where, "SQL"), length(where) == 1)

  stopifnot(DBI::dbExistsTable(con, tbl))

  query <- DBI::SQL(paste0(
    "SHOW",
    if (full) " FULL",
    " COLUMNS",
    paste(" FROM", DBI::dbQuoteIdentifier(con, tbl)),
    if (!is.null(like))
      paste(" LIKE", DBI::dbQuoteString(con, like)),
    if (!is.null(where) && nchar(where) > 0)
      paste(" WHERE", where)
  ))

  res <- tibble::as_tibble(DBI::dbGetQuery(con, query))

  if (parse) {
    tmp <- parse_data_type(res$Type, con = con)
    res <- tibble::as_tibble(cbind(
      res[, "Field"],
      tmp[, c("Type", "Length", "Unsigned")],
      res[, !names(res) %in% c("Field", "Type")]
    ))

    if (!is.logical(res$Null))
      res$Null <- grepl("yes", res$Null, ignore.case = TRUE)

    if ("Collation" %in% names(res)) {
      res <- tibble::add_column(res,
        Charset = sapply(
          strsplit(res$Collation, "_"),
          `[`, 1L
        ),
        .before = "Collation"
      )
    }
  }

  res
}


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

#' @title Parse SQL data type definitions
#'
#' @description Split and parse SQL data type definitions, such as created by
#' any of the data type specification functions (e.g. [col_int], [col_dbl],
#' etc.). The result is a tibble with as many rows as SQL statements were
#' passed and with the following columns:
#'
#' \describe{
#'   \item{Type}{The column data type.}
#'   \item{Length}{Either the length (e.g. CHAR(5) yields 5) or the levels
#'         of an ENUM or SET field.}
#'   \item{Unsigned}{Logical, whether the numeric column is signed.}
#' }
#'
#' @name parse_data_type
#'
#' @param ... Arguments passed to the S3 methods
#' @param con A connection used to determine the SQL dialect to be used
#'
#' @return A tibble.
#'
#' @export
#'
parse_data_type <- function(..., con = get_con()) {
  UseMethod("parse_data_type", con)
}

#' @rdname parse_data_type
#'
#' @param x Character vector to be parsed.
#'
#' @export
#'
parse_data_type.MariaDBConnection <- function(x,
                                              con = get_con(),
                                              ...) {
  get_first <- function(...) sapply(strsplit(...), `[`, 1L)

  get_type <- function(str) get_first(str, "\\s|\\(")

  get_length <- function(str) {
    # extract everything from first "(" to last ")"
    len <- substr(
      str, attr(regexpr("^(.+?)\\(", str), "match.length"),
      attr(regexpr("^(.+)\\)", str), "match.length")
    )
    len[len == ""] <- NA_character_
    names(len) <- NULL
    sub("^\\(", "", sub("\\)$", "", len))
  }

  parse_length <- function(str) {
    str <- tryCatch(as.integer(str), warning = function(w) {
      if (grepl("^NAs introduced by coercion$", conditionMessage(w)))
        str
      else
        warning(w)
    })

    to_parse <- !is.integer(str) & !is.null(str) & !is.na(str)
    if (any(to_parse)) {
      str[to_parse] <- lapply(strsplit(str[to_parse], "',\\s*'"), unquote_str,
        con = con, USE.NAMES = FALSE
      )
      # for some reason, sapply(str, length) causes an error; why!?
      if (all(sapply(str, function(x) length(x)) == 1L)) str <- unlist(str)
    }

    str
  }

  # replace multiple whitespace with single and remove leading whitespace
  x <- sub("^\\s", "", gsub("\\s+", " ", x))

  type <- get_type(x)

  stopifnot(
    type == unquote_ident(type, con),
    type == unquote_str(type, con)
  )

  length <- get_length(x)

  tibble::tibble(
    Type = toupper(type),
    Length = parse_length(length),
    Unsigned = grepl("UNSIGNED", x, ignore.case = TRUE)
  )
}

#' @title Parse SQL column definitions
#'
#' @description Split and parse SQL column definitions, such as created by
#' [tbl_spec].
#'
#' \describe{
#'   \item{Field}{The column name.}
#'   \item{Type, Length, Unsigned}{See [parse_data_type].}
#'   \item{NULL}{Logical, whether the column is nullable.}
#'   \item{Encoding}{The character encoding of a char column.}
#'   \item{Collation}{The collation of a char column.}
#' }
#'
#' @name parse_col_spec
#'
#' @param ... Arguments passed to the S3 methods
#' @param con A connection used to determine the SQL dialect to be used
#'
#' @return A tibble.
#'
#' @export
#'
parse_col_spec <- function(..., con = get_con()) {
  UseMethod("parse_col_spec", con)
}

#' @rdname parse_col_spec
#'
#' @param x Character vector to be parsed.
#'
#' @export
#'
parse_col_spec.MariaDBConnection <- function(x,
                                             con = get_con(),
                                             ...) {
  get_name <- function(str) {
    quoted <- grepl("^`", str)
    nme <- character(length(quoted))

    if (any(quoted))
      nme[quoted] <- sub("`(?:.(?!`))+$", "`", str[quoted], perl = TRUE)
    if (!all(quoted))
      nme[!quoted] <- regmatches(
        str[!quoted],
        regexpr("^([^\\s]+)", str[!quoted],
          perl = TRUE
        )
      )
    nme
  }

  # extract everything that follows regex what until the next whitespace char
  extract_additional <- function(str, what) {
    to_parse <- grepl(what, str)
    res <- character(length(to_parse))
    if (any(to_parse))
      res[to_parse] <- regmatches(
        str[to_parse],
        regexpr(paste0("(?<=", what, "\\s)[^\\s]+"),
          str[to_parse],
          perl = TRUE, ignore.case = TRUE
        )
      )
    res[!to_parse] <- NA_character_
    unquote_str(res, con)
  }

  # replace multiple whitespace with single and remove leading whitespace
  x <- sub("^\\s", "", gsub("\\s+", " ", x))

  nme <- get_name(x)

  x <- mapply(sub,
    pattern = nme, x = x,
    MoreArgs = list(replacement = "", fixed = TRUE)
  )

  type <- parse_data_type(x, con)

  tibble::tibble(
    Field = unquote_ident(nme, con),
    Type = type$Type,
    Length = type$Length,
    Unsigned = type$Unsigned,
    Null = !grepl("NOT NULL", x, ignore.case = TRUE),
    Charset = extract_additional(x, "CHARACTER\\sSET"),
    Collation = extract_additional(x, "COLLATE")
  )
}
