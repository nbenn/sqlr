dialect_registry <- new.env(parent = emptyenv())

#' Register a dialect for a connection class
#'
#' Called by dialect packages from `.onLoad()`, so that [sqlr_for()] can
#' resolve a connection sqlr itself knows nothing about.
#'
#' @param connection_class Name of the `DBIConnection` subclass.
#' @param factory Function of one argument, the connection, returning a
#'   [sqlr_dialect].
#' @param con A `DBIConnection`.
#'
#' @return `sqlr_register_dialect()` returns `NULL` invisibly; `sqlr_for()`
#'   returns a [sqlr_dialect].
#'
#' @export
sqlr_register_dialect <- function(connection_class, factory) {
  stopifnot(is.character(connection_class), is.function(factory))
  assign(connection_class, factory, envir = dialect_registry)
  invisible(NULL)
}

#' @rdname sqlr_register_dialect
#' @export
sqlr_for <- function(con) {
  for (cls in class(con)) {
    if (exists(cls, envir = dialect_registry, inherits = FALSE)) {
      return(get(cls, envir = dialect_registry)(con))
    }
  }

  known <- ls(dialect_registry)
  stop(
    "no sqlr dialect registered for a connection of class \"",
    class(con)[[1L]], "\".\n",
    if (length(known)) {
      paste0("Registered: ", paste0(known, collapse = ", "), ".")
    } else {
      "Load a dialect package such as sqlr.postgres or sqlr.sqlite."
    },
    call. = FALSE
  )
}
