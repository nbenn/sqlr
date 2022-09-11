
#' @export
method(render_sql, list(smallint, con_pq)) <- function(x, con, ...) {
  sql("smallint")
}
