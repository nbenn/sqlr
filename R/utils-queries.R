
#' @title Show columns of db table(s)
#' 
#' @description Display information about the columns in a/the given table(s).
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
#' @param tbls The table name(s).
#' @param like A string, specifying a pattern to match column names against.
#' @param where An SQL expression used to filter results.
#' @param full A logical switch used to request additional information on each
#' of the returned columns.
#' @param con A connection object to connect to the db.
#' 
#' @return A tibble.
#' 
#' @export
#' 
show_db_cols <- function(..., con = get_con()) UseMethod("show_db_cols", con)

#' @export
show_db_cols.MariaDBConnection <- function(tbls,
                                           like = NULL,
                                           where = NULL,
                                           full = FALSE,
                                           con = get_con(),
                                           ...) {

  stopifnot(is.character(tbls), length(tbls) >= 1,
            is.logical(full), length(full) == 1)
  if (!is.null(like))
    stopifnot(is.character(like), length(like) == 1)
  if (!is.null(where))
    stopifnot(inherits(where, "SQL"), length(where) == 1)

  stopifnot(all(sapply(tbls, function(tbl) DBI::dbExistsTable(con, tbl))))

  query <- DBI::SQL(paste0("SHOW",
                           if (full) " FULL",
                           " COLUMNS",
                           if (length(tbls) == 1)
                             paste(" FROM", DBI::dbQuoteIdentifier(con, tbls))
                           else
                             paste(" IN", DBI::dbQuoteIdentifier(con, tbls)),
                           if (!is.null(like) && nchar(like) > 0)
                             paste(" LIKE", DBI::dbQuoteString(con, like)),
                           if (!is.null(where) && nchar(where) > 0)
                             paste(" WHERE", where)
                           ))

  tibble::as_tibble(DBI::dbGetQuery(con, query))
}
