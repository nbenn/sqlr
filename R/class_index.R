#' Standalone indexes
#'
#' An index that is not backing a constraint. Indexes created implicitly by a
#' primary key or unique constraint belong to that constraint's
#' [sqlr_index_spec()] and are not reported here by [sqlr_reflect()].
#'
#' @param name Index name.
#' @param columns Indexed column names.
#' @param desc Per-column descending flags, recycled to `columns`.
#' @param unique Whether the index enforces uniqueness.
#' @param spec An [sqlr_index_spec()] describing method, includes and
#'   predicate.
#' @param description Comment attached to the index.
#' @param attrs Named list of dialect-specific attributes.
#'
#' @return A `sqlr_index` object.
#'
#' @examples
#' sqlr_index("val", name = "orders_val_idx")
#'
#' @export
sqlr_index <- new_class(
  "sqlr_index",
  properties = list(
    name = new_property(class_character, default = NA_character_),
    columns = class_character,
    desc = new_property(class_logical, default = quote(logical())),
    unique = new_property(class_logical, default = FALSE),
    spec = new_property(sqlr_index_spec, default = quote(sqlr_index_spec())),
    description = new_property(class_character, default = NA_character_),
    attrs = new_property(class_list, default = quote(list()))
  ),
  constructor = function(columns, name = NA_character_, desc = FALSE,
                         unique = FALSE, spec = sqlr_index_spec(),
                         description = NA_character_, attrs = list()) {
    new_object(
      S7_object(),
      name = name,
      columns = columns,
      desc = rep_len(desc, length(columns)),
      unique = unique,
      spec = spec,
      description = description,
      attrs = attrs
    )
  },
  validator = function(self) {
    if (!length(self@columns)) {
      return("`columns` must name at least one column")
    }
    if (length(self@desc) != length(self@columns)) {
      "`desc` must be the same length as `columns`"
    }
  }
)
