
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

  stopifnot(is_chr(tbl, n_elem = eq(1L)),
            is_chr(like, n_elem = eq(1L), allow_null = TRUE),
            is_lgl(full, n_elem = eq(1L)),
            is_lgl(parse, n_elem = eq(1L)))
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

    tmp <- parse_col_spec(res$Type, name = FALSE, con = con)
    res <- tibble::as_tibble(cbind(res[, "Field"],
                                   tmp[, c("Type", "Length", "Unsigned")],
                                   res[, !names(res) %in% c("Field", "Type")]))

    if (!is.logical(res$Null))
      res$Null <- grepl("yes", res$Null, ignore.case = TRUE)

    if ("Collation" %in% names(res)) {
      res <- tibble::add_column(res,
                                Charset = sapply(strsplit(res$Collation, "_"),
                                                 `[`, 1L),
                                .before = "Collation")
    }
  }

  res
}
