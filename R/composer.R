
composer <- R6Class(
  "composer",
  public = list(
    generate_sql = function(...) {
      stop("implement in child classes")
    }
  )
)

#' @export
type_composer <- R6Class(
  "type_composer",
  inherit = composer,
  public = list(
    generate_sql = function(x) {
      paste(x$sql_type)
    }
  )
)