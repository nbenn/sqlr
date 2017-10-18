
#' @title Generate a database table
#' 
#' @description All arguments are passed to \code{tbl_spec} which generates
#' SQL for a CREATE TABLE statement, which in turn is executed.
#' 
#' @name create_db_tbl
#' 
#' @param ... Arguments passed to the S3 methods; will end up as the arguments
#' of a call to \code{tbl_spec}
#' @param con A connection used to determine the SQL dialect to be used
#' 
#' @return A single logical value, indicating whether the statement completed
#' successfully (invisibly).
#' 
#' @export
#' 
create_db_tbl <- function(..., con = get_con()) UseMethod("create_db_tbl", con)

#' @rdname create_db_tbl
#' 
#' @export
#' 
create_db_tbl.MariaDBConnection <- function(..., con = get_con()) {

  rs <- DBI::dbSendStatement(con, tbl_spec(..., con = con))
  on.exit(DBI::dbClearResult(rs))

  invisible(DBI::dbHasCompleted(rs))
}

#' @title Delete database table(s)
#' 
#' @description Remove one or more tables from database.
#' 
#' @name drop_db_tbl
#' 
#' @param ... Arguments passed to the S3 methods
#' @param con A connection used to determine the SQL dialect to be used
#' 
#' @return A single logical value, indicating whether the statement completed
#' successfully (invisibly).
#' 
#' @export
#' 
drop_db_tbl <- function(..., con = get_con()) UseMethod("drop_db_tbl", con)

#' @param tbls Character vector indicating which table(s) to delete.
#' @param temporary Logical switch, restricting the delete to temporary tables.
#' @param force Logical switch; if \code{TRUE}, no error is thrown if one of
#' the tables does not exist.
#' 
#' @rdname drop_db_tbl
#' 
#' @export
#' 
drop_db_tbl.MariaDBConnection <- function(tbls,
                                          temporary = FALSE,
                                          force = FALSE,
                                          con = get_con(),
                                          ...) {

  stopifnot(is_chr(tbls),
            is_lgl(temporary, n_elem = eq(1L)),
            is_lgl(force, n_elem = eq(1L)))

  rs <- DBI::dbSendStatement(con,
                             DBI::SQL(paste0("DROP",
                                      if (temporary)
                                        " TEMPORARY",
                                      " TABLE",
                                      if (force)
                                        " IF EXISTS",
                                      paste(DBI::dbQuoteIdentifier(con, tbls),
                                            collapse = ", "))))

  on.exit(DBI::dbClearResult(rs))

  invisible(DBI::dbHasCompleted(rs))
}
