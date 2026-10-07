#' Coerce to a SQL type
#'
#' Accepts a [sqlr_type] unchanged, or parses one of the SQL-standard type
#' spellings below, in any case and spacing. Anything else is an error,
#' including engine aliases such as `int4`, `timestamptz` and `bytea`, so that
#' the accepted set changes only when the types sqlr models do.
#'
#' | Type | Spellings |
#' |---|---|
#' | [sqlr_integer_type] | `smallint`, `integer` or `int`, `bigint` |
#' | [sqlr_float_type] | `real`, `double precision`, `float(p)`: 4 bytes for `p` from 1 to 24, 8 bytes from 25 to 53 |
#' | [sqlr_decimal_type] | `numeric(p, s)` or `decimal(p, s)`, both arguments optional |
#' | [sqlr_string_type] | `char(n)` or `character(n)`, where `n` defaults to 1; `varchar(n)` or `character varying(n)`; `text` |
#' | [sqlr_binary_type] | `binary(n)`, which is fixed-length; `varbinary(n)`; `blob` |
#' | [sqlr_boolean_type] | `boolean` |
#' | [sqlr_time_type] | `date`; `time` and `timestamp`, each with an optional `(p)` and an optional `with time zone` or `without time zone` |
#' | [sqlr_json_type] | `json` |
#' | [sqlr_uuid_type] | `uuid` |
#'
#' A bare `float` is refused, being 8 bytes in some engines and 4 in others.
#' Unsigned and 1-byte integers and binary JSON have no standard spelling, so
#' build them with their constructors, and use [sqlr_other()] to carry any
#' other type verbatim.
#'
#' The `format()` method runs the table in reverse. It gives the standard
#' spelling of a type, or its constructor call if it has none.
#'
#' @param x A `sqlr_type`, or a string naming one.
#'
#' @return An object inheriting from `sqlr_type`.
#'
#' @examples
#' as_sqlr_type("varchar(255)")
#' as_sqlr_type("TIMESTAMP(3) WITH TIME ZONE")
#'
#' format(sqlr_numeric(10, 2))
#' format(sqlr_int(unsigned = TRUE))
#'
#' @export
as_sqlr_type <- function(x) {
  if (S7_inherits(x, sqlr_type)) {
    return(x)
  }

  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop("`x` must be a sqlr_type or a single type name", call. = FALSE)
  }

  type <- standard_type(x)

  if (is.null(type)) {
    quoted <- encodeString(x, quote = "\"")
    stop(
      "unrecognised type ", quoted, ".\n",
      "Use a spelling listed in ?as_sqlr_type, or sqlr_other(", quoted,
      ") to carry it verbatim.",
      call. = FALSE
    )
  }

  type
}

standard_type <- function(x) {
  normalised <- gsub(
    " ?([(),]) ?", "\\1",
    gsub("\\s+", " ", trimws(tolower(x)))
  )
  normalised <- gsub("([),])(?=.)", "\\1 ", normalised, perl = TRUE)

  args <- suppressWarnings(
    as.integer(regmatches(normalised, gregexpr("[0-9]+", normalised))[[1L]])
  )
  if (anyNA(args)) {
    return(NULL)
  }

  arg <- function(i, default = NA_integer_) {
    if (length(args) >= i) args[[i]] else default
  }

  switch(
    gsub("[0-9]+", "#", normalised),
    "smallint" = sqlr_smallint(),
    "int" = ,
    "integer" = sqlr_int(),
    "bigint" = sqlr_bigint(),
    "real" = sqlr_real(),
    "double precision" = sqlr_double(),
    "float(#)" = float_of_precision(arg(1L)),
    "numeric" = ,
    "numeric(#)" = ,
    "numeric(#, #)" = ,
    "decimal" = ,
    "decimal(#)" = ,
    "decimal(#, #)" = sqlr_numeric(arg(1L), arg(2L)),
    "char" = ,
    "char(#)" = ,
    "character" = ,
    "character(#)" = sqlr_char(arg(1L, default = 1L)),
    "varchar(#)" = ,
    "character varying(#)" = sqlr_varchar(arg(1L)),
    "text" = sqlr_text(),
    "binary(#)" = sqlr_binary_type(size = arg(1L), fixed = TRUE),
    "varbinary(#)" = sqlr_blob(arg(1L)),
    "blob" = sqlr_blob(),
    "boolean" = sqlr_boolean(),
    "date" = sqlr_date(),
    "time" = ,
    "time(#)" = ,
    "time without time zone" = ,
    "time(#) without time zone" = sqlr_time(FALSE, arg(1L)),
    "time with time zone" = ,
    "time(#) with time zone" = sqlr_time(TRUE, arg(1L)),
    "timestamp" = ,
    "timestamp(#)" = ,
    "timestamp without time zone" = ,
    "timestamp(#) without time zone" = sqlr_timestamp(FALSE, arg(1L)),
    "timestamp with time zone" = ,
    "timestamp(#) with time zone" = sqlr_timestamp(TRUE, arg(1L)),
    "json" = sqlr_json(),
    "uuid" = sqlr_uuid()
  )
}

float_of_precision <- function(precision) {
  if (precision %in% 1:24) {
    sqlr_real()
  } else if (precision %in% 25:53) {
    sqlr_double()
  }
}

method(format, sqlr_type) <- function(x, ...) constructor_call(x)

method(format, sqlr_integer_type) <- function(x, ...) {
  if (x@unsigned || x@bytes == 1L) {
    return(constructor_call(x))
  }

  switch(
    as.character(x@bytes),
    "2" = "smallint",
    "4" = "integer",
    "8" = "bigint"
  )
}

method(format, sqlr_float_type) <- function(x, ...) {
  if (x@bytes == 4L) "real" else "double precision"
}

method(format, sqlr_decimal_type) <- function(x, ...) {
  if (!is.na(x@precision)) {
    spelling("numeric", x@precision, x@scale)
  } else if (is.na(x@scale)) {
    "numeric"
  } else {
    constructor_call(x)
  }
}

method(format, sqlr_string_type) <- function(x, ...) {
  if (!is.na(x@size)) {
    spelling(if (x@fixed) "char" else "varchar", x@size)
  } else if (!x@fixed) {
    "text"
  } else {
    constructor_call(x)
  }
}

method(format, sqlr_binary_type) <- function(x, ...) {
  if (!is.na(x@size)) {
    spelling(if (x@fixed) "binary" else "varbinary", x@size)
  } else if (!x@fixed) {
    "blob"
  } else {
    constructor_call(x)
  }
}

method(format, sqlr_boolean_type) <- function(x, ...) "boolean"

method(format, sqlr_time_type) <- function(x, ...) {
  if (x@kind != "date") {
    paste0(
      spelling(x@kind, x@precision),
      if (x@with_timezone) " with time zone"
    )
  } else if (!x@with_timezone && is.na(x@precision)) {
    "date"
  } else {
    constructor_call(x)
  }
}

method(format, sqlr_json_type) <- function(x, ...) {
  if (x@binary) constructor_call(x) else "json"
}

method(format, sqlr_uuid_type) <- function(x, ...) "uuid"

spelling <- function(name, ...) {
  args <- c(...)
  args <- args[!is.na(args)]

  if (!length(args)) {
    return(name)
  }

  paste0(name, "(", paste(args, collapse = ", "), ")")
}

constructor_call <- function(x) {
  cls <- S7_class(x)
  props <- setdiff(names(cls@properties), "raw")
  set <- Filter(
    function(p) !identical(prop(x, p), cls@properties[[p]]$default),
    props
  )
  values <- vapply(set, function(p) deparse(prop(x, p)), character(1L))

  paste0(cls@name, "(", paste(set, values, sep = " = ", collapse = ", "), ")")
}
