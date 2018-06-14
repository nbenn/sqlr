
#' @examples
#' small_int$new()$as_sql()
#' # will throw an error
#' # small_int$new()$as_sql(mysql_type_composer$new())
#' 
#' enable_mysql()
#' 
#' mysql_tiny_int$new()$as_sql(mysql_type_composer$new())
#' mysql_tiny_int$new(5)$as_sql(mysql_type_composer$new())
#' 
#' @importFrom assertthat assert_that is.count

#' @export
enable_mysql <- function() {
  numeric$set("private", "display_width", NA)
  numeric$set(
    "public",
    "initialize",
    function(display_width = NA) {
      private$display_width <- display_width
    }
  )
}

#' @export
mysql_type_composer <- R6Class(
  "mysql_type_composer",
  inherit = type_composer,
  public = list(
    dialect = "mysql",
    generate_sql = function(pub, priv) {
      res <- super$generate_sql(pub, priv)
      if (!is.na(priv$display_width)) {
        assert_that(is.count(priv$display_width))
        paste0(res, "(", priv$display_width, ")")
      } else
        res
    }
  )
)

#' @export
mysql_tiny_int <- R6Class(
  "mysql_tiny_int",
  inherit = integer,
  public = list(
    dialect = "mysql"
  ),
  private = list(
    sql_type = "TINYINT"
  )
)

#' @export
mysql_integer <- R6Class(
  "mysql_integer",
  inherit = integer,
  public = list(
    dialect = "mysql"
  )
)

#' @export
mysql_small_int <- R6Class(
  "mysql_small_int",
  inherit = small_int,
  public = list(
    dialect = "mysql"
  )
)

#' @export
mysql_big_int <- R6Class(
  "mysql_big_int",
  inherit = big_int,
  public = list(
    dialect = "mysql"
  )
)
