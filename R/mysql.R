
#' @examples
#' enable_mysql()
#' int <- integer$new(5)
#' int$as_sql(mysql_type_composer$new())
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
    generate_sql = function(x) {
      res <- super$generate_sql(x)
      if (!is.na(x$display_width)) {
        assert_that(is.count(x$display_width))
        paste0(res, "(", x$display_width, ")")
      } else
        res
    }
  )
)

#' @export
mysql_tiny_int <- R6Class(
  "tiny_int",
  inherit = integer,
  private = list(
    sql_type = "TINYINT"
  )
)
