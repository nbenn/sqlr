
if (requireNamespace("RPostgres", quietly = TRUE)) {
  con_pq <- methods::getClass("PqConnection", where = "RPostgres")
} else {
  methods::setClass("PqConnection",
    contains = methods::getClass("DBIConnection", where = "DBI")
  )
  con_pq <- methods::getClass("PqConnection")
}


#' Dummy connection object for PostgreSQL
#' @export
simulate_postgres <- function() {
  methods::new("PqConnection")
}
