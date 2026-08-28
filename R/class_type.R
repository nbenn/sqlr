#' SQL column types
#'
#' A dialect-independent description of a column's type. Each dialect maps
#' these onto its own spelling when rendering, and back again when reflecting.
#' `raw` carries the verbatim spelling a database reported, and is set by
#' [sqlr_reflect()] rather than by hand.
#'
#' `sqlr_other()` carries a type sqlr does not model. It renders verbatim and
#' survives a round trip, but nothing can be inferred about it.
#'
#' @param bytes Width of an integer or float type.
#' @param unsigned Whether an integer type is unsigned.
#' @param size Length of a character or binary type; `NA` for unbounded.
#' @param fixed Whether a character or binary type is blank-padded.
#' @param precision,scale Total digits and digits after the decimal point.
#' @param kind Whether a time type is a `"date"`, `"time"` or `"timestamp"`.
#' @param with_timezone Whether a time type carries a time zone.
#' @param binary Whether JSON is stored in a decomposed binary form.
#' @param name Name of an unmodelled type.
#' @param raw Verbatim type spelling as reported by a database.
#'
#' @return An object inheriting from `sqlr_type`.
#'
#' @examples
#' sqlr_varchar(255)
#' sqlr_numeric(10, 2)
#' sqlr_timestamp(with_timezone = TRUE)
#'
#' @name sqlr_type
NULL

#' @rdname sqlr_type
#' @export
sqlr_type <- new_class(
  "sqlr_type",
  abstract = TRUE,
  properties = list(
    raw = new_property(class_character, default = NA_character_)
  ),
  validator = function(self) {
    if (length(self@raw) != 1L) "`raw` must be a string"
  }
)

#' @rdname sqlr_type
#' @export
sqlr_integer_type <- new_class(
  "sqlr_integer_type",
  parent = sqlr_type,
  properties = list(
    bytes = new_property(class_integer, default = 4L),
    unsigned = new_property(class_logical, default = FALSE)
  ),
  validator = function(self) {
    if (!self@bytes %in% c(1L, 2L, 4L, 8L)) "`bytes` must be 1, 2, 4 or 8"
  }
)

#' @rdname sqlr_type
#' @export
sqlr_float_type <- new_class(
  "sqlr_float_type",
  parent = sqlr_type,
  properties = list(bytes = new_property(class_integer, default = 8L)),
  validator = function(self) {
    if (!self@bytes %in% c(4L, 8L)) "`bytes` must be 4 or 8"
  }
)

#' @rdname sqlr_type
#' @export
sqlr_decimal_type <- new_class(
  "sqlr_decimal_type",
  parent = sqlr_type,
  properties = list(
    precision = new_property(class_integer, default = NA_integer_),
    scale = new_property(class_integer, default = NA_integer_)
  )
)

#' @rdname sqlr_type
#' @export
sqlr_string_type <- new_class(
  "sqlr_string_type",
  parent = sqlr_type,
  properties = list(
    size = new_property(class_integer, default = NA_integer_),
    fixed = new_property(class_logical, default = FALSE)
  )
)

#' @rdname sqlr_type
#' @export
sqlr_binary_type <- new_class(
  "sqlr_binary_type",
  parent = sqlr_type,
  properties = list(
    size = new_property(class_integer, default = NA_integer_),
    fixed = new_property(class_logical, default = FALSE)
  )
)

#' @rdname sqlr_type
#' @export
sqlr_boolean_type <- new_class("sqlr_boolean_type", parent = sqlr_type)

#' @rdname sqlr_type
#' @export
sqlr_time_type <- new_class(
  "sqlr_time_type",
  parent = sqlr_type,
  properties = list(
    kind = new_property(class_character, default = "timestamp"),
    with_timezone = new_property(class_logical, default = FALSE),
    precision = new_property(class_integer, default = NA_integer_)
  ),
  validator = function(self) {
    if (!self@kind %in% c("date", "time", "timestamp")) {
      "`kind` must be \"date\", \"time\" or \"timestamp\""
    }
  }
)

#' @rdname sqlr_type
#' @export
sqlr_json_type <- new_class(
  "sqlr_json_type",
  parent = sqlr_type,
  properties = list(binary = new_property(class_logical, default = FALSE))
)

#' @rdname sqlr_type
#' @export
sqlr_uuid_type <- new_class("sqlr_uuid_type", parent = sqlr_type)

#' @rdname sqlr_type
#' @export
sqlr_other_type <- new_class(
  "sqlr_other_type",
  parent = sqlr_type,
  properties = list(name = class_character),
  validator = function(self) {
    if (length(self@name) != 1L || is.na(self@name)) "`name` must be a string"
  }
)

#' @rdname sqlr_type
#' @export
sqlr_smallint <- function(unsigned = FALSE, raw = NA_character_) {
  sqlr_integer_type(bytes = 2L, unsigned = unsigned, raw = raw)
}

#' @rdname sqlr_type
#' @export
sqlr_int <- function(unsigned = FALSE, raw = NA_character_) {
  sqlr_integer_type(bytes = 4L, unsigned = unsigned, raw = raw)
}

#' @rdname sqlr_type
#' @export
sqlr_bigint <- function(unsigned = FALSE, raw = NA_character_) {
  sqlr_integer_type(bytes = 8L, unsigned = unsigned, raw = raw)
}

#' @rdname sqlr_type
#' @export
sqlr_real <- function(raw = NA_character_) {
  sqlr_float_type(bytes = 4L, raw = raw)
}

#' @rdname sqlr_type
#' @export
sqlr_double <- function(raw = NA_character_) {
  sqlr_float_type(bytes = 8L, raw = raw)
}

#' @rdname sqlr_type
#' @export
sqlr_numeric <- function(precision = NA, scale = NA, raw = NA_character_) {
  sqlr_decimal_type(
    precision = as.integer(precision),
    scale = as.integer(scale),
    raw = raw
  )
}

#' @rdname sqlr_type
#' @export
sqlr_varchar <- function(size = NA, raw = NA_character_) {
  sqlr_string_type(size = as.integer(size), fixed = FALSE, raw = raw)
}

#' @rdname sqlr_type
#' @export
sqlr_char <- function(size = NA, raw = NA_character_) {
  sqlr_string_type(size = as.integer(size), fixed = TRUE, raw = raw)
}

#' @rdname sqlr_type
#' @export
sqlr_text <- function(raw = NA_character_) {
  sqlr_string_type(size = NA_integer_, fixed = FALSE, raw = raw)
}

#' @rdname sqlr_type
#' @export
sqlr_blob <- function(size = NA, raw = NA_character_) {
  sqlr_binary_type(size = as.integer(size), raw = raw)
}

#' @rdname sqlr_type
#' @export
sqlr_boolean <- function(raw = NA_character_) sqlr_boolean_type(raw = raw)

#' @rdname sqlr_type
#' @export
sqlr_date <- function(raw = NA_character_) {
  sqlr_time_type(kind = "date", raw = raw)
}

#' @rdname sqlr_type
#' @export
sqlr_time <- function(with_timezone = FALSE, precision = NA,
                      raw = NA_character_) {
  sqlr_time_type(
    kind = "time",
    with_timezone = with_timezone,
    precision = as.integer(precision),
    raw = raw
  )
}

#' @rdname sqlr_type
#' @export
sqlr_timestamp <- function(with_timezone = FALSE, precision = NA,
                           raw = NA_character_) {
  sqlr_time_type(
    kind = "timestamp",
    with_timezone = with_timezone,
    precision = as.integer(precision),
    raw = raw
  )
}

#' @rdname sqlr_type
#' @export
sqlr_json <- function(binary = FALSE, raw = NA_character_) {
  sqlr_json_type(binary = binary, raw = raw)
}

#' @rdname sqlr_type
#' @export
sqlr_uuid <- function(raw = NA_character_) sqlr_uuid_type(raw = raw)

#' @rdname sqlr_type
#' @export
sqlr_other <- function(name, raw = NA_character_) {
  sqlr_other_type(name = name, raw = raw)
}
