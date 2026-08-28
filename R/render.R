method(sqlr_defers_constraints, sqlr_dialect) <- function(dialect, ...) TRUE

method(sqlr_quote, sqlr_dialect) <- function(dialect, x, ...) {
  paste0("\"", gsub("\"", "\"\"", x, fixed = TRUE), "\"")
}

method(sqlr_quote_literal, sqlr_dialect) <- function(dialect, x, ...) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) {
    return("NULL")
  }
  if (is.logical(x)) {
    return(if (x) "TRUE" else "FALSE")
  }
  if (is.numeric(x)) {
    return(format(x, scientific = FALSE, trim = TRUE))
  }
  paste0("'", gsub("'", "''", as.character(x), fixed = TRUE), "'")
}

method(sqlr_render, list(sqlr_column, sqlr_dialect)) <-
  function(x, dialect, ...) {
    parts <- c(sqlr_quote(dialect, x@name), sqlr_render_type(x@type, dialect))

    if (!is.null(x@identity)) {
      parts <- c(parts, render_identity(x@identity, dialect))
    }

    if (!x@null) {
      parts <- c(parts, "NOT NULL")
    }

    if (!is.null(x@default)) {
      parts <- c(parts, paste("DEFAULT", render_value(x@default, dialect)))
    }

    paste(parts, collapse = " ")
  }

render_identity <- function(x, dialect) {
  generated <- if (x@generated == "always") "ALWAYS" else "BY DEFAULT"
  options <- character()

  if (!identical(x@start, 1L)) {
    options <- c(options, paste("START WITH", x@start))
  }
  if (!identical(x@increment, 1L)) {
    options <- c(options, paste("INCREMENT BY", x@increment))
  }

  paste0(
    "GENERATED ", generated, " AS IDENTITY",
    if (length(options)) paste0(" (", paste(options, collapse = " "), ")")
  )
}

render_value <- function(x, dialect) {
  if (S7_inherits(x, sqlr_sql)) x@text else sqlr_quote_literal(dialect, x)
}

render_columns <- function(columns, dialect) {
  paste0(sqlr_quote(dialect, columns), collapse = ", ")
}

constraint_prefix <- function(x, dialect) {
  if (is.na(x@name)) "" else paste0("CONSTRAINT ", sqlr_quote(dialect, x@name), " ")
}

method(sqlr_render, list(sqlr_primary_key, sqlr_dialect)) <-
  function(x, dialect, ...) {
    paste0(
      constraint_prefix(x, dialect),
      "PRIMARY KEY (", render_columns(x@columns, dialect), ")"
    )
  }

method(sqlr_render, list(sqlr_unique, sqlr_dialect)) <-
  function(x, dialect, ...) {
    paste0(
      constraint_prefix(x, dialect),
      "UNIQUE (", render_columns(x@columns, dialect), ")"
    )
  }

method(sqlr_render, list(sqlr_check, sqlr_dialect)) <-
  function(x, dialect, ...) {
    paste0(constraint_prefix(x, dialect), "CHECK (", x@expr@text, ")")
  }

method(sqlr_render, list(sqlr_foreign_key, sqlr_dialect)) <-
  function(x, dialect, ..., qualifier = NA_character_) {
    schema <- if (is.na(x@ref_schema)) qualifier else x@ref_schema
    target <- qualified_name(x@ref_table, schema, dialect)

    out <- paste0(
      constraint_prefix(x, dialect),
      "FOREIGN KEY (", render_columns(x@columns, dialect), ") ",
      "REFERENCES ", target, " (", render_columns(x@ref_columns, dialect), ")"
    )

    if (x@on_delete != "no action") {
      out <- paste0(out, " ON DELETE ", toupper(x@on_delete))
    }
    if (x@on_update != "no action") {
      out <- paste0(out, " ON UPDATE ", toupper(x@on_update))
    }

    out
  }

method(sqlr_render, list(sqlr_table, sqlr_dialect)) <-
  function(x, dialect, ..., qualifier = NA_character_) {
    c(
      render_create_table(x, dialect, x@constraints, qualifier),
      render_table_indexes(x, dialect, qualifier)
    )
  }

render_create_table <- function(x, dialect, constraints, qualifier) {
  body <- c(
    vapply(x@columns, sqlr_render, character(1L), dialect = dialect),
    vapply(
      constraints, sqlr_render, character(1L),
      dialect = dialect, qualifier = qualifier
    )
  )

  paste0(
    "CREATE TABLE ", qualified_name(x@name, qualifier, dialect), " (\n  ",
    paste0(body, collapse = ",\n  "), "\n)"
  )
}

render_table_indexes <- function(x, dialect, qualifier) {
  vapply(
    x@indexes,
    function(idx) sqlr_render(idx, dialect, table = x@name, qualifier = qualifier),
    character(1L)
  )
}

method(sqlr_render, list(sqlr_index, sqlr_dialect)) <-
  function(x, dialect, ..., table, qualifier = NA_character_) {
    keys <- paste0(
      sqlr_quote(dialect, x@columns),
      ifelse(x@desc, " DESC", ""),
      collapse = ", "
    )

    paste0(
      "CREATE ", if (x@unique) "UNIQUE " else "", "INDEX ",
      if (!is.na(x@name)) paste0(sqlr_quote(dialect, x@name), " "),
      "ON ", qualified_name(table, qualifier, dialect), " (", keys, ")",
      if (!is.null(x@spec@where)) paste0(" WHERE ", x@spec@where@text)
    )
  }

qualified_name <- function(name, qualifier, dialect) {
  if (is.na(qualifier)) {
    sqlr_quote(dialect, name)
  } else {
    paste0(sqlr_quote(dialect, qualifier), ".", sqlr_quote(dialect, name))
  }
}

method(sqlr_render, list(sqlr_schema, sqlr_dialect)) <-
  function(x, dialect, ...) {
    if (!length(x@tables)) {
      return(character())
    }

    hoisted <- if (sqlr_defers_constraints(dialect)) {
      cyclic_foreign_keys(x)
    } else {
      lapply(x@tables, function(tbl) rep(FALSE, length(tbl@constraints)))
    }
    order <- table_order(x, hoisted)
    qualifier <- x@name

    creates <- unlist(lapply(order, function(i) {
      tbl <- x@tables[[i]]
      inline <- tbl@constraints[!hoisted[[i]]]
      c(
        render_create_table(tbl, dialect, inline, qualifier),
        render_table_indexes(tbl, dialect, qualifier)
      )
    }))

    alters <- unlist(lapply(order, function(i) {
      tbl <- x@tables[[i]]
      vapply(
        tbl@constraints[hoisted[[i]]],
        function(con) {
          paste0(
            "ALTER TABLE ", qualified_name(tbl@name, qualifier, dialect),
            " ADD ", sqlr_render(con, dialect, qualifier = qualifier)
          )
        },
        character(1L)
      )
    }))

    c(creates, alters)
  }
