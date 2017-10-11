
#' @title Generate SQL for a boolean data type definition
#' 
#' @description Generate SQL, that can be used for boolean data type
#' specifications in column definitions for CREATE/ALTER TABLE statements.
#' 
#' @inheritParams col_spec
#' 
#' @return SQL to be used in a CREATE table statement
#' 
#' @export
#' 
show_db_cols <- function(..., con = get_con()) UseMethod("show_db_cols", con)

#' @export
show_db_cols.MariaDBConnection <- function(tbls,
                                           like = NULL,
                                           where = NULL,
                                           full = FALSE,
                                           ...) {

  stopifnot(is.character(tbls), length(tbls) >= 1,
            is.character(like), length(like) == 1,
            inherits(where, "SQL"), length(where) == 1,
            is.logical(full), length(full) == 1)

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

  DBI::dbGetQuery(con, query)
}
