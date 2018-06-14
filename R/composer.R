
composer <- R6Class(
  "composer",
  public = list(
    dialect = "sql:2011",
    generate_sql = function(...) {
      stop("implement in child classes")
    }
  ),
  private = list(
    check_dialect = function(x) {
      assert_that(
        isTRUE(x == self$dialect),
        msg = "No support from the currently enabled dialect."
      )
    }
  )
)

#' @export
type_composer <- R6Class(
  "type_composer",
  inherit = composer,
  public = list(
    generate_sql = function(pub, priv) {
      private$check_dialect(pub$dialect)
      paste(priv$sql_type)
    }
  )
)