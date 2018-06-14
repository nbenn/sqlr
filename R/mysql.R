
#' @examples
#' enable_mysql()
#' int <- integer$new(5)
#' int$as_sql(mysql_type_composer$new())
#' 
#' @importFrom assertthat assert_that is.count

#' @export
enable_mysql <- function() {
  numeric$set("private", "length", NA)
  numeric$set(
    "public",
    "initialize",
    function(length = NA) {
      private$length <- length
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
      if (!is.na(x$length)) {
        assert_that(is.count(x$length))
        paste0(res, "(", x$length, ")")
      } else
        res
    }
  )
)