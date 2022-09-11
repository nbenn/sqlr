
#' Integer types
#' @param .data R7 doc
#' @export
int <- new_class("int", parent = class_integer)

#' @rdname int
#' @export
smallint <- new_class("smallint", parent = class_integer,
  validator = function(self) {
    dat <- R7_data(self)
    if (any(dat < -32768L) || any(dat > 32767L)) {
      "value range exceeded"
    }
  }
)
