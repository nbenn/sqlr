#' SQL dialects
#'
#' A dialect is what rendering and reflection dispatch on. It is deliberately
#' not the connection: a dialect object can be built without a driver package
#' installed, which keeps sqlr free of database dependencies and lets the whole
#' test suite run with no server.
#'
#' `sqlr_dialect` is abstract. It carries the rendering every engine shares,
#' which concrete dialects in companion packages inherit and override. It is
#' never instantiated and never used as a fallback, so there is no generic
#' pseudo-dialect emitting SQL that no engine has been tested against.
#'
#' @param version Engine version, governing version-dependent rendering.
#' @param attrs Named list of dialect-specific attributes.
#'
#' @return An object inheriting from `sqlr_dialect`.
#'
#' @export
sqlr_dialect <- new_class(
  "sqlr_dialect",
  abstract = TRUE,
  properties = list(
    version = new_property(class_character, default = NA_character_),
    attrs = new_property(class_list, default = quote(list()))
  )
)
