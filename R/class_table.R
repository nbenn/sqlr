#' Tables
#'
#' Columns, constraints and indexes are passed positionally in any order.
#' Columns named in a primary key have `null` forced to `FALSE`, which the SQL
#' standard implies and every engine reports back, so an authored table
#' compares equal to its own reflection.
#'
#' @param name Table name.
#' @param ... [sqlr_column()]s, [sqlr_constraint]s and [sqlr_index()]es.
#' @param description Comment attached to the table.
#' @param attrs Named list of dialect-specific attributes.
#'
#' @return A `sqlr_table` object.
#'
#' @examples
#' sqlr_table(
#'   "users",
#'   sqlr_column("id", sqlr_bigint()),
#'   sqlr_column("email", "varchar(255)", null = FALSE),
#'   sqlr_primary_key("id")
#' )
#'
#' @export
sqlr_table <- new_class(
  "sqlr_table",
  properties = list(
    name = class_character,
    columns = new_property(class_list, default = quote(list())),
    constraints = new_property(class_list, default = quote(list())),
    indexes = new_property(class_list, default = quote(list())),
    description = new_property(class_character, default = NA_character_),
    attrs = new_property(class_list, default = quote(list()))
  ),
  constructor = function(name, ..., description = NA_character_,
                         attrs = list()) {
    parts <- list(...)
    new_object(
      S7_object(),
      name = name,
      columns = imply_not_null(
        keep_class(parts, sqlr_column),
        keep_class(parts, sqlr_constraint)
      ),
      constraints = keep_class(parts, sqlr_constraint),
      indexes = keep_class(parts, sqlr_index),
      description = description,
      attrs = attrs
    )
  },
  validator = function(self) {
    if (length(self@name) != 1L || is.na(self@name)) {
      return("`name` must be a string")
    }

    nms <- column_names(self)
    if (anyDuplicated(nms)) {
      return("column names must be unique")
    }

    unknown <- setdiff(constrained_columns(self), nms)
    if (length(unknown)) {
      paste0(
        "constraints and indexes name unknown columns: ",
        paste0(unknown, collapse = ", ")
      )
    }
  }
)

keep_class <- function(x, cls) {
  x[vapply(x, S7_inherits, logical(1L), class = cls)]
}

column_names <- function(x) {
  vapply(x@columns, function(col) col@name, character(1L))
}

constrained_columns <- function(x) {
  from_constraints <- lapply(x@constraints, function(con) {
    if (S7_inherits(con, sqlr_check)) character() else con@columns
  })
  unique(unlist(c(from_constraints, lapply(x@indexes, function(i) i@columns))))
}

imply_not_null <- function(columns, constraints) {
  keys <- keep_class(constraints, sqlr_primary_key)
  if (!length(keys)) {
    return(columns)
  }

  key_columns <- unlist(lapply(keys, function(k) k@columns))
  lapply(columns, function(col) {
    if (col@name %in% key_columns) col@null <- FALSE
    col
  })
}
