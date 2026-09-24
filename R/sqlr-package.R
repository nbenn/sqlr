#' @import S7
#' @keywords internal
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}
