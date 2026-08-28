#' Coerce to a SQL type
#'
#' Accepts a [sqlr_type] unchanged, or parses a string spelling such as
#' `"varchar(255)"` or `"numeric(10, 2)"`. Spellings sqlr does not recognise
#' become [sqlr_other()] and are carried through verbatim.
#'
#' @param x A `sqlr_type`, or a string naming one.
#'
#' @return An object inheriting from `sqlr_type`.
#'
#' @examples
#' as_sqlr_type("varchar(255)")
#' as_sqlr_type("geometry")
#'
#' @export
as_sqlr_type <- function(x) {
  if (S7_inherits(x, sqlr_type)) {
    return(x)
  }

  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop("`x` must be a sqlr_type or a single type name", call. = FALSE)
  }

  spec <- parse_type_spelling(x)
  build_type_from_spelling(spec$name, spec$args, x)
}

parse_type_spelling <- function(x) {
  trimmed <- trimws(x)
  open <- regexpr("(", trimmed, fixed = TRUE)

  if (open == -1L) {
    return(list(name = tolower(trimmed), args = integer()))
  }

  close <- utils::tail(gregexpr(")", trimmed, fixed = TRUE)[[1L]], 1L)
  if (close < open) {
    stop("unbalanced parentheses in type `", x, "`", call. = FALSE)
  }

  inner <- substr(trimmed, open + 1L, close - 1L)
  args <- suppressWarnings(
    as.integer(trimws(strsplit(inner, ",", fixed = TRUE)[[1L]]))
  )

  list(name = tolower(trimws(substr(trimmed, 1L, open - 1L))), args = args)
}

build_type_from_spelling <- function(name, args, raw) {
  arg <- function(i) if (length(args) >= i) args[[i]] else NA_integer_

  switch(
    gsub("[[:space:]]+", " ", name),
    "smallint" = ,
    "int2" = sqlr_smallint(raw = raw),
    "int" = ,
    "integer" = ,
    "int4" = sqlr_int(raw = raw),
    "bigint" = ,
    "int8" = sqlr_bigint(raw = raw),
    "real" = ,
    "float4" = sqlr_real(raw = raw),
    "double" = ,
    "double precision" = ,
    "float8" = sqlr_double(raw = raw),
    "numeric" = ,
    "decimal" = sqlr_numeric(arg(1L), arg(2L), raw = raw),
    "varchar" = ,
    "character varying" = sqlr_varchar(arg(1L), raw = raw),
    "char" = ,
    "character" = ,
    "bpchar" = sqlr_char(arg(1L), raw = raw),
    "text" = sqlr_text(raw = raw),
    "blob" = ,
    "bytea" = ,
    "binary" = sqlr_blob(arg(1L), raw = raw),
    "bool" = ,
    "boolean" = sqlr_boolean(raw = raw),
    "date" = sqlr_date(raw = raw),
    "time" = sqlr_time(precision = arg(1L), raw = raw),
    "timetz" = ,
    "time with time zone" = sqlr_time(TRUE, arg(1L), raw = raw),
    "timestamp" = sqlr_timestamp(precision = arg(1L), raw = raw),
    "timestamptz" = ,
    "timestamp with time zone" = sqlr_timestamp(TRUE, arg(1L), raw = raw),
    "json" = sqlr_json(raw = raw),
    "jsonb" = sqlr_json(TRUE, raw = raw),
    "uuid" = sqlr_uuid(raw = raw),
    sqlr_other(name, raw = raw)
  )
}
