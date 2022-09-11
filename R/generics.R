
#' Generate SQL
#'
#' For a given SQL dialect determined by `con`, SQL is generated corresponding
#' to the object passed as `x`.
#'
#' @param x Object
#' @param con Database connection
#'
#' @return SQL string
#'
#' @export
render_sql <- new_generic("render_sql", c("x", "con"))

#' Parse SQL
#'
#' The inverse operation as [render_sql()], i.e. instantiating an {sqlr} object
#' from an SQL string.
#'
#' @param x SQL string
#' @param con Database connection
#'
#' @return {sqlr} object
#'
#' @export
parse_sql <- new_generic("parse_sql", c("x", "con"))
