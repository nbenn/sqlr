#' Verbatim SQL
#'
#' Wraps a string that is to be emitted as-is, without quoting or escaping.
#' Accepted anywhere a value, expression or type may appear.
#'
#' @param text String of SQL.
#'
#' @return An object of class `sqlr_sql`.
#'
#' @examples
#' sqlr_sql("now()")
#'
#' @export
sqlr_sql <- new_class(
  "sqlr_sql",
  properties = list(text = class_character),
  validator = function(self) {
    if (length(self@text) != 1L || is.na(self@text)) "`text` must be a string"
  }
)

method(print, sqlr_sql) <- function(x, ...) {
  cat("<sqlr_sql> ", x@text, "\n", sep = "")
  invisible(x)
}

as_sql_text <- function(x) {
  if (S7_inherits(x, sqlr_sql)) x@text else as.character(x)
}
