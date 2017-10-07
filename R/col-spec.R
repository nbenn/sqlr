
#' @title Generate SQL for a column definition
#' 
#' @param ... Arguments passed to the S3 methods
#' @param con A connection used to determine the SQL dialect to be used
#' 
#' @return SQL to be used in a CREATE table statement
#' 
#' @export
#' 
col_spec <- function(..., con = get_con()) UseMethod("col_spec", con)

#' @export
col_spec.MariaDBConnection <- function(name,
                                       type,
                                       nullable = TRUE,
                                       default = NULL,
                                       auto_increment = FALSE,
                                       key = c(NA, "unique", "primary", "key"),
                                       comment = NULL,
                                       con = get_con(),
                                       ...) {

  stopifnot(is.character(name), length(name) == 1,
            "SQL" %in% class(type), length(type) == 1,
            is.logical(nullable), length(nullable) == 1,
            is.logical(auto_increment), length(auto_increment) == 1)
  if (!is.null(default)) stopifnot(is.character(default), length(default) == 1)
  if (!is.null(comment)) stopifnot(is.character(comment), length(comment) == 1)

  key <- match.arg(key)
  key <- switch(key,
                unique = "UNIQUE KEY",
                primary = "PRIMARY KEY",
                key = "KEY")

  DBI::SQL(paste(DBI::dbQuoteIdentifier(con, name),
                 type,
                 if (!nullable)
                   "NOT NULL",
                 if (!is.null(default))
                   paste("DEFAULT", default),
                 if (auto_increment)
                   "AUTO_INCREMENT",
                 if (!is.null(key))
                   key,
                 if (!is.null(comment))
                   paste("COMMENT", DBI::dbQuoteString(comment))))
}

#' @title Generate SQL for an integer data type definition
#' 
#' @inheritParams col_spec
#' 
#' @return SQL to be used in a CREATE table statement
#' 
#' @export
#' 
col_int <- function(..., con = get_con()) UseMethod("col_int", con)

#' @export
col_int.MariaDBConnection <- function(type = "int",
                                      unsigned = FALSE,
                                      min = NA,
                                      max = NA,
                                      range = c(min, max),
                                      ...) {

  if (!any(is.na(range))) {

    stopifnot(is.numeric(range), length(range) == 2L)

    min <- min(range)
    max <- max(range)

    if (!missing(type)) warning("param \"type\" will be ignored.")
    if (!missing(unsigned)) warning("param \"unsigned\" will be ignored.")

    if (is.na(min)) min <- bit64::as.integer64(-2147483648)
    if (is.na(max)) max <- 2147483647L

    unsigned <- min >= 0

    if      ( (min >=     -128L & max <      128L) |
              (min >=        0L & max <      256L) ) type <- "tiny"
    else if ( (min >=   -32768L & max <    32768L) |
              (min >=        0L & max <    65536L) ) type <- "small"
    else if ( (min >= -8388608L & max <  8388608L) |
              (min >=        0L & max < 16777216L) ) type <- "medium"
    else if ( (min >= bit64::as.integer64(-2147483648) &
               max <  2147483647L) |
              (min >= 0L        &
               max <  bit64::as.integer64(4294967296))) type <- "int"
    else type <- "big"

  } else {

    stopifnot(length(type) == 1L,
              any(c("tiny", "small", "medium", "int", "big") %in% type),
              is.logical(unsigned), length(unsigned) == 1L)

    if (!missing(min)) warning("param \"min\" will be ignored.")
    if (!missing(max)) warning("param \"max\" will be ignored.")
    if (!missing(range)) warning("param \"range\" will be ignored.")

  }

  type <- switch(type,
                 tiny   = "TINYINT",
                 small  = "SMALLINT",
                 medium = "MEDIUMINT",
                 int    = "INT",
                 big    = "BIGINT")

  DBI::SQL(paste0(type, if (unsigned) " UNSIGNED"))
}

#' @title Generate SQL for floating point data type definition
#' 
#' @inheritParams col_spec
#' 
#' @return SQL to be used in a CREATE table statement
#' 
#' @export
#' 
col_dbl <- function(..., con = get_con()) UseMethod("col_dbl", con)

#' @export
col_dbl.MariaDBConnection <- function(prec = "double",
                                      unsigned = FALSE,
                                      ...) {

  stopifnot(length(prec) == 1L, any(c("single", "double") %in% prec),
            length(unsigned) == 1L, is.logical(unsigned))

  prec <- switch(prec,
                 single = "FLOAT",
                 double = "DOUBLE")

  DBI::SQL(paste0(prec, if (unsigned) " UNSIGNED"))
}

#' @title Generate SQL for a string data type definition
#' 
#' @inheritParams col_spec
#' 
#' @return SQL to be used in a CREATE table statement
#' 
#' @export
#' 
col_chr <- function(..., con = get_con()) UseMethod("col_chr", con)

#' @export
col_chr.MariaDBConnection <- function(length = 255L,
                                      fixed = FALSE,
                                      force_text = length >= 16384L,
                                      char_set = NA_character_,
                                      collate = NA_character_,
                                      con = get_con(),
                                      ...) {

  stopifnot(is.numeric(length), length(length) == 1L, length > 0L,
            is.logical(fixed), length(fixed) == 1L,
            is.logical(force_text), length(force_text) == 1L,
            is.character(char_set), length(char_set) == 1L,
            is.character(collate), length(collate) == 1L)

  if (length < 256L) {

    if (fixed) {
      stopifnot(length > 256L)
      type <- paste0("CHAR(", length, ")")
    } else if (force_text)
      type <- "TINYTEXT"
    else
      type <- paste0("VARCHAR(", length, ")")

  } else if (length < 65536L) {

    if (force_text)
      type <- "TEXT"
    else {
      type <- paste0("VARCHAR(", length, ")")
      if (length >= 16384L) {
        warning("effective max length might be lower than 2^16; ",
                "possibly consider TEXT instead")
      }
    }
  } else if (length < 16777216L)
    type <- "MEDIUMTEXT"
  else
    type <- "LONGTEXT"

  DBI::SQL(paste(type,
                 if (!is.na(char_set))
                   paste("CHARACTER SET", DBI::dbQuoteString(con, char_set)),
                 if (!is.na(collate))
                   paste("COLLATE", DBI::dbQuoteString(con, collate))))
}

#' @title Generate SQL for a binary data type definition
#' 
#' @inheritParams col_spec
#' 
#' @return SQL to be used in a CREATE table statement
#' 
#' @export
#' 
col_raw <- function(..., con = get_con()) UseMethod("col_raw", con)

#' @export
col_raw.MariaDBConnection <- function(length = 255L,
                                      fixed = FALSE,
                                      force_blob = length >= 16384L,
                                      ...) {

  stopifnot(is.numeric(length), length(length) == 1L, length > 0L,
            is.logical(fixed), length(fixed) == 1L,
            is.logical(force_text), length(force_text) == 1L)

  if (length < 256L) {

    if (fixed) {
      stopifnot(length > 256L)
      type <- paste0("BINARY(", length, ")")
    } else if (force_text)
      type <- "TINYBLOB"
    else
      type <- paste0("VARBINARY(", length, ")")

  } else if (length < 65536L) {

    if (force_text)
      type <- "BLOB"
    else {
      type <- paste0("VARBINARY(", length, ")")
      if (length >= 16384L) {
        warning("effective max length might be lower than 2^16; ",
                "possibly consider BLOB instead")
      }
    }
  } else if (length < 16777216L)
    type <- "MEDIUMBLOB"
  else
    type <- "LONGBLOB"

  DBI::SQL(type)
}

#' @title Generate SQL for a boolean data type definition
#' 
#' @inheritParams col_spec
#' 
#' @return SQL to be used in a CREATE table statement
#' 
#' @export
#' 
col_lgl <- function(..., con = get_con()) UseMethod("col_lgl", con)

#' @export
col_lgl.MariaDBConnection <- function(...) DBI::SQL("BOOL")

#' @title Generate SQL for a factor data type definition
#' 
#' @inheritParams col_spec
#' 
#' @return SQL to be used in a CREATE table statement
#' 
#' @export
#' 
col_fct <- function(..., con = get_con()) UseMethod("col_fct", con)

#' @export
col_fct.MariaDBConnection <- function(levels,
                                      type = c("enum", "set"),
                                      char_set = NA_character_,
                                      collate = NA_character_,
                                      con = get_con(),
                                      ...) {

  stopifnot(!missing(levels),
            is.character(levels), length(levels) >= 1)

  type <- match.arg(type)
  levels <- unique(levels)

  levels <- DBI::dbQuoteString(con, levels)

  type <- paste0(toupper(type),
                 "(",
                   paste(levels, collapse = ", "),
                 ")")

  DBI::SQL(paste(type,
                 if (!is.na(char_set))
                   paste("CHARACTER SET", DBI::dbQuoteString(con, char_set)),
                 if (!is.na(collate))
                   paste("COLLATE", DBI::dbQuoteString(con, collate))))
}

#' @title Generate SQL for a date/time data type definition
#' 
#' @inheritParams col_spec
#' 
#' @return SQL to be used in a CREATE table statement
#' 
#' @export
#' 
col_dtm <- function(..., con = get_con()) UseMethod("col_dtm", con)

#' @export
col_dtm.MariaDBConnection <- function(type = c("datetime", "date", "time",
                                               "year"),
                                      val = NULL,
                                      ...) {

  if (!is.null(val)) {

    if (!missing(type)) warning("param \"type\" will be ignored.")

    type <- class(val)

    if ("POSIXt" %in% type)
      type <- "datetime"
    else if ("Date" %in% type)
      type <- "date"
    else if ("difftime" %in% type)
      type <- "time"
    else if ("numeric" %in% type) {
      val <- as.integer(val)
      if (length(val) == 0)
        type <- "year"
      else if (nchar(val) %in% c(2, 4))
        type <- paste0("year(", nchar(val), ")")
      else
        stop("numbers are treated as years and only 2 or 4 digits are ",
             "accepted")
    } else
      stop("class ", type, " is not recognized for determining col_dtm")

  } else {

    type <- match.arg(type)

  }

  DBI::SQL(toupper(type))
}