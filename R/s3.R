new_sqlr <- function(x, subclass) {
  structure(x, class = c(paste0("sqlr_", subclass), "sqlr"))
}

#' @export
sqlr_render <- function(x, con, ...) UseMethod("sqlr_render")
