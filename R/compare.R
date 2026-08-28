#' Compare two schemas
#'
#' Compares structurally rather than by identity: constraints and indexes are
#' matched on what they constrain, not on their order or their names, and the
#' verbatim type spelling a database reported is ignored. This is what lets a
#' schema authored in R be compared against the same schema read back out of a
#' database with [sqlr_reflect()].
#'
#' Check constraints are matched by name where both carry one. Their
#' expressions are not compared: engines rewrite them on the way in, inserting
#' casts that depend on the column's type, and undoing that reliably would take
#' a full expression parser.
#'
#' @param x,y Objects to compare.
#'
#' @return `sqlr_diff()` a character vector of differences, empty when equal;
#'   `sqlr_equal()` a flag.
#'
#' @examples
#' a <- sqlr_table("t", sqlr_column("id", sqlr_int()))
#' sqlr_equal(a, a)
#'
#' @export
sqlr_diff <- function(x, y) {
  out <- if (S7_inherits(x, sqlr_schema)) {
    diff_schema(x, y)
  } else if (S7_inherits(x, sqlr_table)) {
    diff_table(x, y, "")
  } else {
    stop("`x` must be a sqlr_schema or sqlr_table", call. = FALSE)
  }

  if (is.null(out)) character() else out
}

#' @rdname sqlr_diff
#' @export
sqlr_equal <- function(x, y) length(sqlr_diff(x, y)) == 0L

diff_schema <- function(x, y) {
  in_x <- table_names(x)
  in_y <- table_names(y)

  out <- c(
    if (length(setdiff(in_x, in_y))) {
      paste0("missing tables: ", paste0(setdiff(in_x, in_y), collapse = ", "))
    },
    if (length(setdiff(in_y, in_x))) {
      paste0("extra tables: ", paste0(setdiff(in_y, in_x), collapse = ", "))
    }
  )

  shared <- intersect(in_x, in_y)
  c(out, unlist(lapply(shared, function(nm) {
    diff_table(schema_table(x, nm), schema_table(y, nm), paste0(nm, ": "))
  })))
}

diff_table <- function(x, y, prefix) {
  c(
    diff_columns(x, y, prefix),
    diff_set(
      constraint_keys(x@constraints), constraint_keys(y@constraints),
      prefix, "constraint"
    ),
    diff_set(index_keys(x@indexes), index_keys(y@indexes), prefix, "index")
  )
}

diff_columns <- function(x, y, prefix) {
  in_x <- column_names(x)
  in_y <- column_names(y)

  if (!identical(in_x, in_y)) {
    return(paste0(
      prefix, "columns differ: [", paste0(in_x, collapse = ", "),
      "] vs [", paste0(in_y, collapse = ", "), "]"
    ))
  }

  unlist(lapply(seq_along(x@columns), function(i) {
    diff_column(x@columns[[i]], y@columns[[i]], prefix)
  }))
}

diff_column <- function(x, y, prefix) {
  where <- paste0(prefix, "column ", x@name, ": ")

  c(
    if (!identical(type_key(x@type), type_key(y@type))) {
      paste0(where, "type ", type_key(x@type), " vs ", type_key(y@type))
    },
    if (!identical(x@null, y@null)) {
      paste0(where, "null ", x@null, " vs ", y@null)
    },
    if (!identical(default_key(x@default), default_key(y@default))) {
      paste0(
        where, "default ", default_key(x@default), " vs ",
        default_key(y@default)
      )
    },
    if (!identical(is.null(x@identity), is.null(y@identity))) {
      paste0(where, "identity present on one side only")
    }
  )
}

type_key <- function(x) {
  props <- setdiff(names(props(x)), "raw")
  paste0(
    class(x)[[1L]], "(",
    paste0(
      props, "=",
      vapply(props, function(p) paste0(prop(x, p), collapse = "/"), character(1L)),
      collapse = ", "
    ),
    ")"
  )
}

default_key <- function(x) {
  if (is.null(x)) "none" else as_sql_text(x)
}

constraint_keys <- function(constraints) {
  sort(vapply(constraints, constraint_key, character(1L)))
}

constraint_key <- function(x) {
  if (S7_inherits(x, sqlr_primary_key)) {
    paste0("primary_key(", paste0(x@columns, collapse = ","), ")")
  } else if (S7_inherits(x, sqlr_unique)) {
    paste0("unique(", paste0(x@columns, collapse = ","), ")")
  } else if (S7_inherits(x, sqlr_foreign_key)) {
    paste0(
      "foreign_key(", paste0(x@columns, collapse = ","), "->",
      x@ref_table, "(", paste0(x@ref_columns, collapse = ","), ")",
      ",on_delete=", x@on_delete, ",on_update=", x@on_update, ")"
    )
  } else {
    paste0("check(", if (is.na(x@name)) x@expr@text else x@name, ")")
  }
}

index_keys <- function(indexes) {
  sort(vapply(
    indexes,
    function(x) {
      paste0(
        if (x@unique) "unique_index(" else "index(",
        paste0(x@columns, ifelse(x@desc, " desc", ""), collapse = ","), ")"
      )
    },
    character(1L)
  ))
}

diff_set <- function(in_x, in_y, prefix, label) {
  c(
    if (length(setdiff(in_x, in_y))) {
      paste0(prefix, "missing ", label, ": ",
             paste0(setdiff(in_x, in_y), collapse = "; "))
    },
    if (length(setdiff(in_y, in_x))) {
      paste0(prefix, "extra ", label, ": ",
             paste0(setdiff(in_y, in_x), collapse = "; "))
    }
  )
}
