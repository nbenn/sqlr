#' Schemas and catalogues
#'
#' A schema is a named collection of tables; a catalogue holds several schemas,
#' which is what [sqlr_reflect()] returns when asked for more than one and what
#' cross-schema foreign keys resolve against.
#'
#' @param name Schema name.
#' @param ... [sqlr_table()]s, or [sqlr_schema()]s for `sqlr_catalog()`.
#' @param description Comment attached to the schema.
#' @param attrs Named list of dialect-specific attributes.
#'
#' @return A `sqlr_schema` or `sqlr_catalog` object.
#'
#' @examples
#' sqlr_schema("public", sqlr_table("t", sqlr_column("id", sqlr_int())))
#'
#' @export
sqlr_schema <- new_class(
  "sqlr_schema",
  properties = list(
    name = new_property(class_character, default = NA_character_),
    tables = new_property(class_list, default = quote(list())),
    description = new_property(class_character, default = NA_character_),
    attrs = new_property(class_list, default = quote(list()))
  ),
  constructor = function(name = NA_character_, ...,
                         description = NA_character_, attrs = list()) {
    new_object(
      S7_object(),
      name = name,
      tables = keep_class(list(...), sqlr_table),
      description = description,
      attrs = attrs
    )
  },
  validator = function(self) {
    nms <- table_names(self)
    if (anyDuplicated(nms)) "table names must be unique"
  }
)

#' @rdname sqlr_schema
#' @export
sqlr_catalog <- new_class(
  "sqlr_catalog",
  properties = list(
    schemas = new_property(class_list, default = quote(list())),
    attrs = new_property(class_list, default = quote(list()))
  ),
  constructor = function(..., attrs = list()) {
    new_object(
      S7_object(),
      schemas = keep_class(list(...), sqlr_schema),
      attrs = attrs
    )
  }
)

table_names <- function(x) {
  vapply(x@tables, function(tbl) tbl@name, character(1L))
}

schema_table <- function(x, name) {
  hit <- match(name, table_names(x))
  if (is.na(hit)) NULL else x@tables[[hit]]
}
