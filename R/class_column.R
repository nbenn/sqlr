#' Table columns
#'
#' A column of a [sqlr_table()]. `identity` describes an automatically
#' generated key; it is what a dialect maps its `SERIAL`-style spellings onto.
#'
#' @param name Column name.
#' @param type A [sqlr_type], or a string parsed by [as_sqlr_type()].
#' @param null Whether the column admits `NULL`. A column named in a primary
#'   key is forced to `FALSE`, matching what every engine reports back.
#' @param default Default value: a length-one atomic, a [sqlr_sql()]
#'   expression, or `NULL` for none.
#' @param description Comment attached to the column.
#' @param identity An [sqlr_identity()], or `NULL`.
#' @param attrs Named list of dialect-specific attributes, ignored by dialects
#'   that do not recognise them.
#' @param generated Whether the identity always applies, or yields to a
#'   supplied value.
#' @param start,increment Sequence start and step.
#'
#' @return A `sqlr_column` or `sqlr_identity` object.
#'
#' @examples
#' sqlr_column("email", "varchar(255)", null = FALSE)
#' sqlr_column("id", sqlr_bigint(), identity = sqlr_identity())
#'
#' @export
sqlr_identity <- new_class(
  "sqlr_identity",
  properties = list(
    generated = new_property(class_character, default = "by default"),
    start = new_property(class_integer, default = 1L),
    increment = new_property(class_integer, default = 1L)
  ),
  validator = function(self) {
    if (!self@generated %in% c("always", "by default")) {
      "`generated` must be \"always\" or \"by default\""
    }
  }
)

#' @rdname sqlr_identity
#' @export
sqlr_column <- new_class(
  "sqlr_column",
  properties = list(
    name = class_character,
    type = sqlr_type,
    null = new_property(class_logical, default = TRUE),
    default = new_property(class_any, default = NULL),
    description = new_property(class_character, default = NA_character_),
    identity = new_property(new_union(NULL, sqlr_identity), default = NULL),
    attrs = new_property(class_list, default = quote(list()))
  ),
  constructor = function(name, type, null = TRUE, default = NULL,
                         description = NA_character_, identity = NULL,
                         attrs = list()) {
    new_object(
      S7_object(),
      name = name,
      type = as_sqlr_type(type),
      null = null,
      default = default,
      description = description,
      identity = identity,
      attrs = attrs
    )
  },
  validator = function(self) {
    if (length(self@name) != 1L || is.na(self@name)) {
      return("`name` must be a string")
    }
    if (length(self@null) != 1L || is.na(self@null)) {
      return("`null` must be TRUE or FALSE")
    }
    if (!valid_default(self@default)) {
      "`default` must be NULL, a length-one atomic, or a sqlr_sql()"
    }
  }
)

valid_default <- function(x) {
  is.null(x) || S7_inherits(x, sqlr_sql) || (is.atomic(x) && length(x) == 1L)
}
