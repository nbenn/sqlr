
#' @title Generate SQL for a column definition
#'
#' @description Generate SQL, that can be used for column definitions in
#' CREATE/ALTER TABLE statements.
#'
#' @name col_spec
#'
#' @param ... Arguments passed to the S3 methods
#'
#' @return SQL to be used in a CREATE table statement
#'
#' @param name String specifying the column name
#' @param type Either a call to one of the functions capable of specifying a
#' column type (where the con object will be replaced with the one from the
#' enclosing function call), or the name of one of the functions (as string
#' or function object) with arguments specified in the enclosing function call.
#' @param nullable Logical switch specifying whether the column may contain
#' NULL or not.
#' @param default String specifying the default value.
#' @param auto_increment Logical switch specifying whether the column is auto
#' incrementing.
#' @param key One of \code{NA}, \code{unique}, \code{primary}, \code{key},
#' specifying if and what type of index the column represents.
#' @param comment An optional comment string.
#'
#' @examples
#' \dontrun{
#' col_spec(name = "foo", type = col_int())
#' col_spec(name = "foo", type = col_int(unsigned = TRUE))
#'
#' col_spec(name = "foo", type = col_int)
#' col_spec(name = "foo", type = col_int, unsigned = TRUE)
#'
#' col_spec(name = "foo", type = "integer")
#' col_spec(name = "foo", type = "integer", unsigned = TRUE)
#'
#' col_spec(name = "foo", type = "int")
#' col_spec(name = "foo", type = "int", unsigned = TRUE)
#' }
#'
#' @export
#'
col_spec <- function(name = paste(sample(letters, 10, TRUE),
                       collapse = ""
                     ),
                     type = col_int(),
                     nullable = TRUE,
                     default = NULL,
                     auto_increment = FALSE,
                     key = c(NA, "unique", "primary", "key"),
                     comment = NULL,
                     ...) {
  key <- match.arg(key)

  if (is_chr(type)) {
    stopifnot(is_chr(type, n_elem = eq(1L)))
    type <-
      switch(type,
        col_int = ,
        integer = ,
        int = col_int,
        col_dbl = ,
        double = ,
        numeric = col_dbl,
        col_chr = ,
        character = ,
        char = col_chr,
        col_raw = ,
        raw = col_raw,
        col_lgl = ,
        logical = col_lgl,
        col_fct = ,
        factor = col_fct,
        col_dtm = ,
        dtm = ,
        datetime = col_dtm,
        stop("Unknown type: ", type, call. = FALSE)
      )
  }

  if (is.function(type)) {
    type <- type(...)
  }
  # FIXME: Check class of return type

  stopifnot(
    is_chr(name, n_elem = eq(1L)),
    is_lgl(nullable, n_elem = eq(1L)),
    is_lgl(auto_increment, n_elem = eq(1L)),
    is_vec(default,
      n_elem = eq(1L), allow_na = TRUE,
      allow_null = TRUE
    ),
    is_chr(comment, n_elem = eq(1L), allow_null = TRUE)
    # FIXME: cols inherit from "sqlr_col"
  )

  if (!is.null(default) && is.na(default)) default <- NA_character_

  key <- switch(key,
    unique = " UNIQUE KEY",
    primary = " PRIMARY KEY",
    key = " KEY"
  )

  obj <- as.list(environment())
  new_sqlr(obj, subclass = "col_spec")
}

#' @export sqlr_render.sqlr_col_spec
#' @method sqlr_render sqlr_col_spec
#' @export
#' @rdname sqlr_render
sqlr_render.sqlr_col_spec <- function(x, con, ...) UseMethod("sqlr_render.sqlr_col_spec", con)

#' @method sqlr_render.sqlr_col_spec MariaDBConnection
#' @export
sqlr_render.sqlr_col_spec.MariaDBConnection <- function(x, con, ...) {
  list2env(x, environment())

  DBI::SQL(paste0(
    DBI::dbQuoteIdentifier(con, name),
    " ", sqlr_render(type, con),
    if (!nullable) {
      " NOT NULL"
    },
    if (is_chr(default, n_char = NULL, allow_na = TRUE)) {
      paste(" DEFAULT", DBI::dbQuoteString(con, default))
    } else if (is_num(default)) {
      paste(" DEFAULT", default)
    } else if (is_lgl(default)) {
      paste(" DEFAULT", as.numeric(default))
    },
    if (auto_increment) {
      " AUTO_INCREMENT"
    },
    if (!is.null(key)) {
      key
    },
    if (!is.null(comment) && nchar(comment) > 0) {
      paste(" COMMENT", DBI::dbQuoteString(con, comment))
    }
  ))
}

#' @title Generate SQL for an integer data type definition
#'
#' @description Generate SQL, that can be used for integer data type
#' specifications in column definitions for CREATE/ALTER TABLE statements.
#'
#' @name col_int
#'
#' @inheritParams col_spec
#'
#' @return SQL to be used in a CREATE table statement
#'
#' @param size One of \code{tiny}, \code{small}, \code{medium}, \code{int},
#' \code{big}, specifying the size of integer.
#' @param unsigned Logical switch specifying whether the integer is singed or
#' unsigned.
#' @param min,max The minimum/maximum values the integer column has to be able
#' to hold. This can be used for inferring size and signedness automatically.
#'
#' @export
#'
col_int <- function(size = "int",
                    unsigned = FALSE,
                    min = NA,
                    max = NA,
                    ...) {
  if (!is.na(min) | !is.na(max)) {
    if (is.na(min)) {
      min <- bit64::as.integer64(-2147483648)
    } else {
      stopifnot(is_int(min, n_elem = eq(1L), strict = FALSE))
    }
    if (is.na(max)) {
      max <- 2147483647L
    } else {
      stopifnot(is_int(max, n_elem = eq(1L), strict = FALSE))
    }

    stopifnot(min <= max)

    if (!missing(size)) warning("param \"size\" will be ignored.")
    if (!missing(unsigned)) warning("param \"unsigned\" will be ignored.")

    unsigned <- min >= 0

    if      ( (min >=     -128L & max <      128L) |
              (min >=        0L & max <      256L) ) size <- "tiny"
    else if ( (min >=   -32768L & max <    32768L) |
              (min >=        0L & max <    65536L) ) size <- "small"
    else if ( (min >= -8388608L & max <  8388608L) |
              (min >=        0L & max < 16777216L) ) size <- "medium"
    else if ( (min >= bit64::as.integer64(-2147483648) &
               max <  bit64::as.integer64(2147483648)) |
              (min >= 0L        &
               max <  bit64::as.integer64(4294967296))) size <- "int"
    else size <- "big"
  } else {
    stopifnot(
      is_chr(size, n_elem = eq(1L)),
      any(c("tiny", "small", "medium", "int", "big") %in% size),
      is_lgl(unsigned, n_elem = eq(1L))
    )
  }

  obj <- list(size = size, unsigned = unsigned)
  new_sqlr(obj, subclass = "col_int")
}

#' @export sqlr_render.sqlr_col_int
#' @method sqlr_render sqlr_col_int
#' @export
#' @rdname sqlr_render
sqlr_render.sqlr_col_int <- function(x, con, ...) UseMethod("sqlr_render.sqlr_col_int", con)

#' @method sqlr_render.sqlr_col_int MariaDBConnection
#' @export
sqlr_render.sqlr_col_int.MariaDBConnection <- function(x, con, ...) {
  list2env(x, environment())

  size <- switch(size,
    tiny = "TINYINT",
    small = "SMALLINT",
    medium = "MEDIUMINT",
    int = "INT",
    big = "BIGINT"
  )

  DBI::SQL(paste0(size, if (unsigned) " UNSIGNED"))
}

#' @title Generate SQL for floating point data type definition
#'
#' @description Generate SQL, that can be used for floating point data type
#' specifications in column definitions for CREATE/ALTER TABLE statements.
#'
#' @name col_dbl
#'
#' @inheritParams col_spec
#'
#' @return SQL to be used in a CREATE table statement
#'
#' @param prec One of \code{single}, \code{double}, specifying the floating
#' point precision the column is expected to be capable of holding.
#' @param unsigned Logical switch specifying whether the values are singed or
#' unsigned.
#'
#' @export
#'
col_dbl <- function(prec = "double",
                    unsigned = FALSE,
                    ...) {
  obj <- as.list(environment())
  new_sqlr(obj, subclass = "col_dbl")
}

#' @export sqlr_render.sqlr_col_dbl
#' @method sqlr_render sqlr_col_dbl
#' @export
#' @rdname sqlr_render
sqlr_render.sqlr_col_dbl <- function(x, con, ...) UseMethod("sqlr_render.sqlr_col_dbl", con)

#' @method sqlr_render.sqlr_col_dbl MariaDBConnection
#' @export
sqlr_render.sqlr_col_dbl.MariaDBConnection <- function(x, con, ...) {
  list2env(x, environment())

  stopifnot(
    is_chr(prec, n_elem = eq(1L)),
    any(c("single", "double") %in% prec),
    is_lgl(unsigned, n_elem = eq(1L))
  )

  prec <- switch(prec,
    single = "FLOAT",
    double = "DOUBLE"
  )

  DBI::SQL(paste0(prec, if (unsigned) " UNSIGNED"))
}

#' @title Generate SQL for a string data type definition
#'
#' @description Generate SQL, that can be used for string data type
#' specifications in column definitions for CREATE/ALTER TABLE statements.
#'
#' @name col_chr
#'
#' @inheritParams col_spec
#'
#' @return SQL to be used in a CREATE table statement
#'
#' @export
#'
#' @param length The maximum length of string the column is expected to hold.
#' @param fixed Logical switch specifying whether strings are fixed length
#' or varying in length.
#' @param force_text String length threshold for forcing the type from
#' (VAR)CHAR to TEXT which causes the values to be stored separately from the
#' rest of the row. TEXT columns only contribute 9 to 12 bytes toward the row
#' size limit (65,535 bytes).
#' @param char_set,collate The character set and collation used.
#'
#' @section TODO: implement a function show_db_charset that fetches SHOW
#' CHARACTER SET and use the results for checking if the char_set is listed.
#' Furthermore, SHOW COLLATION WHERE Charset = 'char_set' can be used to test
#' whether the selected collation is allowed for the given charset. A warning
#' could be generated if the user selects a charset with Maxlen > 1: max row
#' size becomes smaller and char wastes space

#' @export
#'
col_chr <- function(length = 255L,
                    fixed = FALSE,
                    force_text = length >= 16384L,
                    char_set = NA_character_,
                    collate = NA_character_,
                    ...) {
  obj <- as.list(environment())
  new_sqlr(obj, subclass = "col_chr")
}

#' @export sqlr_render.sqlr_col_chr
#' @method sqlr_render sqlr_col_chr
#' @export
#' @rdname sqlr_render
sqlr_render.sqlr_col_chr <- function(x, con, ...) UseMethod("sqlr_render.sqlr_col_chr", con)

#' @method sqlr_render.sqlr_col_chr MariaDBConnection
#' @export
sqlr_render.sqlr_col_chr.MariaDBConnection <- function(x, con, ...) {
  list2env(x, environment())

  stopifnot(
    is_int(length, n_elem = eq(1L), strict = FALSE), length > 0L,
    is_lgl(fixed, n_elem = eq(1L)),
    is_lgl(force_text, n_elem = eq(1L)),
    is_chr(char_set, n_elem = eq(1L), allow_na = TRUE),
    is_chr(collate, n_elem = eq(1L), allow_na = TRUE),
    !all(c(fixed, force_text))
  )
  if (fixed) stopifnot(length < 256L)

  length <- as.integer(length)

  if (length < 256L) {
    if (fixed) {
      type <- paste0("CHAR(", length, ")")
    } else if (force_text) {
      type <- "TINYTEXT"
    } else {
      type <- paste0("VARCHAR(", length, ")")
    }
  } else if (length < 65536L) {
    if (force_text) {
      type <- "TEXT"
    } else {
      type <- paste0("VARCHAR(", length, ")")
      if (length >= 16384L) {
        warning(
          "effective max length might be lower than 2^16; ",
          "possibly consider TEXT instead"
        )
      }
    }
  } else if (length < 16777216L) {
    type <- "MEDIUMTEXT"
  } else {
    type <- "LONGTEXT"
  }

  DBI::SQL(paste0(
    type,
    if (!is.na(char_set)) {
      paste(" CHARACTER SET", DBI::dbQuoteString(con, char_set))
    },
    if (!is.na(collate)) {
      paste(" COLLATE", DBI::dbQuoteString(con, collate))
    }
  ))
}

#' @title Generate SQL for a binary data type definition
#'
#' @description Generate SQL, that can be used for binary data type
#' specifications in column definitions for CREATE/ALTER TABLE statements.
#'
#' @name col_raw
#'
#' @inheritParams col_spec
#'
#' @return SQL to be used in a CREATE table statement
#'
#' @param length The maximum number of bytes the column is expected to hold.
#' @param fixed Logical switch specifying whether lengths are fixed throughout
#' rows.
#' @param force_blob Length threshold for forcing the type from (VAR)BINARY to
#' BLOB which causes the values to be stored separately from the rest of the
#' row. BLOB columns only contribute 9 to 12 bytes toward the row size limit
#' (65,535 bytes).
#'
#' @export
#'
col_raw <- function(length = 255L,
                    fixed = FALSE,
                    force_blob = length >= 16384L,
                    ...) {
  obj <- as.list(environment())
  new_sqlr(obj, subclass = "col_raw")
}

#' @export sqlr_render.sqlr_col_raw
#' @method sqlr_render sqlr_col_raw
#' @export
#' @rdname sqlr_render
sqlr_render.sqlr_col_raw <- function(x, con, ...) UseMethod("sqlr_render.sqlr_col_raw", con)

#' @method sqlr_render.sqlr_col_raw MariaDBConnection
#' @export
sqlr_render.sqlr_col_raw.MariaDBConnection <- function(x, con, ...) {
  list2env(x, environment())

  stopifnot(
    is_int(length, n_elem = eq(1L), strict = FALSE), length > 0L,
    is_lgl(fixed, n_elem = eq(1L)),
    is_lgl(force_blob, n_elem = eq(1L)),
    !all(c(fixed, force_blob))
  )
  if (fixed) stopifnot(length < 256L)

  if (length < 256L) {
    if (fixed) {
      type <- paste0("BINARY(", length, ")")
    } else if (force_blob) {
      type <- "TINYBLOB"
    } else {
      type <- paste0("VARBINARY(", length, ")")
    }
  } else if (length < 65536L) {
    if (force_blob) {
      type <- "BLOB"
    } else {
      type <- paste0("VARBINARY(", length, ")")
      if (length >= 16384L) {
        warning(
          "effective max length might be lower than 2^16; ",
          "possibly consider BLOB instead"
        )
      }
    }
  } else if (length < 16777216L) {
    type <- "MEDIUMBLOB"
  } else {
    type <- "LONGBLOB"
  }

  DBI::SQL(type)
}

#' @title Generate SQL for a boolean data type definition
#'
#' @description Generate SQL, that can be used for boolean data type
#' specifications in column definitions for CREATE/ALTER TABLE statements.
#'
#' @inheritParams col_spec
#'
#' @return SQL to be used in a CREATE table statement
#'
#' @export
col_lgl <- function(...) {
  obj <- as.list(environment())
  new_sqlr(obj, subclass = "col_lgl")
}

#' @export sqlr_render.sqlr_col_lgl
#' @method sqlr_render sqlr_col_lgl
#' @export
#' @rdname sqlr_render
sqlr_render.sqlr_col_lgl <- function(x, con, ...) UseMethod("sqlr_render.sqlr_col_lgl", con)

#' @method sqlr_render.sqlr_col_lgl MariaDBConnection
#' @export
sqlr_render.sqlr_col_lgl.MariaDBConnection <- function(x, con, ...) {
  list2env(x, environment())

  DBI::SQL("BOOL")
}

#' @title Generate SQL for a factor data type definition
#'
#' @description Generate SQL, that can be used for factor data type
#' specifications in column definitions for CREATE/ALTER TABLE statements.
#'
#' @name col_fct
#'
#' @inheritParams col_spec
#'
#' @return SQL to be used in a CREATE table statement
#'
#' @param levels A character vector, specifying the factor levels.
#' @param variant One of \code{enum}, \code{set}.
#' @param char_set,collate The character set and collation used.
#'
#' @export
#'
col_fct <- function(levels,
                    variant = c("enum", "set"),
                    char_set = NA_character_,
                    collate = NA_character_,
                    ...) {
  variant <- match.arg(variant)

  obj <- as.list(environment())
  new_sqlr(obj, subclass = "col_fct")
}

#' @export sqlr_render.sqlr_col_fct
#' @method sqlr_render sqlr_col_fct
#' @export
#' @rdname sqlr_render
sqlr_render.sqlr_col_fct <- function(x, con, ...) UseMethod("sqlr_render.sqlr_col_fct", con)

#' @method sqlr_render.sqlr_col_fct MariaDBConnection
#' @export
sqlr_render.sqlr_col_fct.MariaDBConnection <- function(x, con, ...) {
  list2env(x, environment())

  stopifnot(is_chr(levels, n_elem = gte(1L), n_char = NULL))

  levels <- unique(levels)

  levels <- DBI::dbQuoteString(con, levels)

  variant <- paste0(
    toupper(variant),
    "(",
    paste(levels, collapse = ", "),
    ")"
  )

  DBI::SQL(paste0(
    variant,
    if (!is.na(char_set) && nchar(char_set) > 0) {
      paste(" CHARACTER SET", DBI::dbQuoteString(con, char_set))
    },
    if (!is.na(collate) && nchar(collate) > 0) {
      paste(" COLLATE", DBI::dbQuoteString(con, collate))
    }
  ))
}

#' @title Generate SQL for a date/time data type definition
#'
#' @description Generate SQL, that can be used for date/time data type
#' specifications in column definitions for CREATE/ALTER TABLE statements.
#'
#' @inheritParams col_spec
#'
#' @return SQL to be used in a CREATE table statement
#'
#' @param class One of \code{datetime}, \code{date}, \code{time}, \code{year},
#' specifying the type of date/time of the column.
#' @param val A value, the class of which is used for automatically inferring
#' type.
#'
#' @export
#'
col_dtm <- function(class = c(
                      "datetime", "date", "time",
                      "year"
                    ),
                    val = NULL,
                    ...) {
  if (!is.null(val)) {
    if (!missing(class)) warning("param \"class\" will be ignored.")

    class <- class(val)

    if ("POSIXt" %in% class) {
      class <- "datetime"
    } else if ("Date" %in% class) {
      class <- "date"
    } else if ("difftime" %in% class) {
      class <- "time"
    } else if ("numeric" %in% class | "integer" %in% class) {
      val <- as.integer(val)
      if (length(val) == 0 || nchar(val) == 4) {
        class <- "year"
      } else {
        stop("numbers are treated as years and only 4 digits are accepted")
      }
    } else {
      stop("class ", class, " is not recognized for determining col_dtm")
    }
  } else {
    class <- match.arg(class)
  }

  obj <- as.list(environment())
  new_sqlr(obj, subclass = "col_dtm")
}

#' @export sqlr_render.sqlr_col_dtm
#' @method sqlr_render sqlr_col_dtm
#' @export
#' @rdname sqlr_render
sqlr_render.sqlr_col_dtm <- function(x, con, ...) UseMethod("sqlr_render.sqlr_col_dtm", con)

#' @method sqlr_render.sqlr_col_dtm MariaDBConnection
#' @export
sqlr_render.sqlr_col_dtm.MariaDBConnection <- function(x, con, ...) {
  list2env(x, environment())

  DBI::SQL(toupper(class))
}

#' @title Generate SQL for an id column
#'
#' @description Generate SQL, that can be used for an id column specification
#' (typically an auto incrementing, unsigned integer type primary key column),
#' in column definitions for CREATE/ALTER TABLE statements.
#'
#' @name col_id
#'
#' @param ... Arguments passed to the S3 methods
#'
#' @return SQL to be used in a CREATE table statement
#'
#' @inheritParams col_spec
#' @inheritParams col_int
#' @param as_lst Logical switch for returning the col_spec as a list entry for
#' easier c'ing with col_specs from a data.frame
#'
#' @export
#'
# FIXME: Rename to id_col_spec()
col_id <- function(name = "id",
                   type = "int",
                   unsigned = TRUE,
                   auto_increment = TRUE,
                   key = "primary",
                   as_lst = FALSE,
                   ...) {
  obj <- as.list(environment())

  spec <- col_spec(
    name = name, type = type, unsigned = unsigned,
    nullable = FALSE, auto_increment = auto_increment,
    key = key, ...
  )

  if (as_lst) {
    stats::setNames(list(spec), name)
  } else {
    spec
  }
}
