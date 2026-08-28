#' Table constraints
#'
#' Primary keys and unique constraints own an [sqlr_index_spec()] describing
#' the index that backs them, rather than being modelled as indexes carrying a
#' flag. This mirrors the database catalogue, where the constraint owns its
#' supporting index, so a reflected schema maps onto the same shape.
#'
#' Constraint identity is structural -- the columns, referenced target or
#' expression. `name` is a renderable attribute, so a schema authored here and
#' the same schema read back from a database compare equal even when the
#' engine chose its own names.
#'
#' @param name Constraint name. `NA` leaves naming to the dialect.
#' @param columns Names of the constrained columns.
#' @param ref_table,ref_schema,ref_columns Foreign key target.
#' @param on_delete,on_update Referential action: one of `"no action"`,
#'   `"restrict"`, `"cascade"`, `"set null"` or `"set default"`.
#' @param expr Check expression, as a string or [sqlr_sql()].
#' @param spec An [sqlr_index_spec()] describing the backing index.
#' @param method Index method, such as `"btree"`.
#' @param include Non-key columns carried in the index.
#' @param where Partial index predicate.
#' @param description Comment attached to the constraint.
#' @param attrs Named list of dialect-specific attributes.
#'
#' @return An object inheriting from `sqlr_constraint`.
#'
#' @examples
#' sqlr_primary_key("id")
#' sqlr_foreign_key("user_id", "users", "id", on_delete = "cascade")
#' sqlr_check("total > 0", name = "orders_total_check")
#'
#' @name sqlr_constraint
NULL

#' @rdname sqlr_constraint
#' @export
sqlr_index_spec <- new_class(
  "sqlr_index_spec",
  properties = list(
    method = new_property(class_character, default = NA_character_),
    include = new_property(class_character, default = quote(character())),
    where = new_property(new_union(NULL, sqlr_sql), default = NULL),
    attrs = new_property(class_list, default = quote(list()))
  )
)

#' @rdname sqlr_constraint
#' @export
sqlr_constraint <- new_class(
  "sqlr_constraint",
  abstract = TRUE,
  properties = list(
    name = new_property(class_character, default = NA_character_),
    description = new_property(class_character, default = NA_character_),
    attrs = new_property(class_list, default = quote(list()))
  )
)

#' @rdname sqlr_constraint
#' @export
sqlr_primary_key <- new_class(
  "sqlr_primary_key",
  parent = sqlr_constraint,
  properties = list(
    columns = class_character,
    spec = new_property(sqlr_index_spec, default = quote(sqlr_index_spec()))
  ),
  constructor = function(columns, name = NA_character_,
                         spec = sqlr_index_spec(),
                         description = NA_character_, attrs = list()) {
    new_object(
      S7_object(),
      name = name,
      description = description,
      attrs = attrs,
      columns = columns,
      spec = spec
    )
  },
  validator = function(self) {
    if (!length(self@columns)) "`columns` must name at least one column"
  }
)

#' @rdname sqlr_constraint
#' @export
sqlr_unique <- new_class(
  "sqlr_unique",
  parent = sqlr_constraint,
  properties = list(
    columns = class_character,
    spec = new_property(sqlr_index_spec, default = quote(sqlr_index_spec()))
  ),
  constructor = function(columns, name = NA_character_,
                         spec = sqlr_index_spec(),
                         description = NA_character_, attrs = list()) {
    new_object(
      S7_object(),
      name = name,
      description = description,
      attrs = attrs,
      columns = columns,
      spec = spec
    )
  },
  validator = function(self) {
    if (!length(self@columns)) "`columns` must name at least one column"
  }
)

referential_actions <- c(
  "no action", "restrict", "cascade", "set null", "set default"
)

#' @rdname sqlr_constraint
#' @export
sqlr_foreign_key <- new_class(
  "sqlr_foreign_key",
  parent = sqlr_constraint,
  properties = list(
    columns = class_character,
    ref_table = class_character,
    ref_columns = class_character,
    ref_schema = new_property(class_character, default = NA_character_),
    on_delete = new_property(class_character, default = "no action"),
    on_update = new_property(class_character, default = "no action")
  ),
  constructor = function(columns, ref_table, ref_columns,
                         ref_schema = NA_character_,
                         on_delete = "no action", on_update = "no action",
                         name = NA_character_, description = NA_character_,
                         attrs = list()) {
    new_object(
      S7_object(),
      name = name,
      description = description,
      attrs = attrs,
      columns = columns,
      ref_table = ref_table,
      ref_columns = ref_columns,
      ref_schema = ref_schema,
      on_delete = tolower(on_delete),
      on_update = tolower(on_update)
    )
  },
  validator = function(self) {
    if (!length(self@columns)) {
      return("`columns` must name at least one column")
    }
    if (length(self@columns) != length(self@ref_columns)) {
      return("`columns` and `ref_columns` must be the same length")
    }
    if (length(self@ref_table) != 1L || is.na(self@ref_table)) {
      return("`ref_table` must be a string")
    }
    bad <- setdiff(c(self@on_delete, self@on_update), referential_actions)
    if (length(bad)) {
      paste0(
        "referential action must be one of ",
        paste0("\"", referential_actions, "\"", collapse = ", ")
      )
    }
  }
)

#' @rdname sqlr_constraint
#' @export
sqlr_check <- new_class(
  "sqlr_check",
  parent = sqlr_constraint,
  properties = list(expr = sqlr_sql),
  constructor = function(expr, name = NA_character_,
                         description = NA_character_, attrs = list()) {
    if (!S7_inherits(expr, sqlr_sql)) expr <- sqlr_sql(text = expr)
    new_object(
      S7_object(),
      name = name,
      description = description,
      attrs = attrs,
      expr = expr
    )
  }
)
