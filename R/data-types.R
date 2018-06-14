#' @examples
#' int <- integer$new()
#' int$as_sql()
#' 
#' @importFrom R6 R6Class

data_type <- R6Class(
  "data_type",
  public = list(
    dialect = "sql:2011",
    as_r = function(...) {
      stop("implement in child classes")
    },
    as_sql = function(composer = type_composer$new()) {
      composer$generate_sql(self, private)
    }
  ),
  private = list(
    sql_type = NULL
  )
)

numeric <- R6Class(
  "numeric",
  inherit = data_type
)

#' @export
integer <- R6Class(
  "integer",
  inherit = numeric,
  public = list(
    as_r = as.integer
  ),
  private = list(
    sql_type = "INTEGER"
  )
)

#' @export
small_int <- R6Class(
  "small_int",
  inherit = integer,
  private = list(
    sql_type = "SMALLINT"
  )
)

#' @export
big_int <- R6Class(
  "big_int",
  inherit = integer,
  private = list(
    sql_type = "BIGINT"
  )
)
