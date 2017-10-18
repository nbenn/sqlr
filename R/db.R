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
#' @return SQL to be used in a CREATE table statement
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
  DBI::dbHasCompleted(rs)
}
