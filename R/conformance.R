#' Dialect conformance tests
#'
#' A dialect package calls `sqlr_test_types()` from its test suite. It
#' registers one test per entry of `sqlr_conformance_types()`. Each renders a
#' table holding a single column of that type, creates it on a live database,
#' reflects it and expects an empty [sqlr_diff()]. The list covers every type
#' class sqlr models and each parameter it carries, and lives here rather than
#' in the dialects, so a type added to sqlr is tested in every dialect without
#' anyone adding it to a test.
#'
#' A dialect that cannot represent a type declares it in `skip`, with a reason,
#' and the test for that type is skipped citing it.
#'
#' @param connect Function of no arguments returning a `DBIConnection`, or
#'   skipping when no database is available. It is called once per type, and
#'   the connection is closed afterwards. The dialect is resolved by
#'   [sqlr_for()], and the table is created in the connection's default schema
#'   and removed again.
#' @param skip Named character vector: each name is an entry of
#'   `sqlr_conformance_types()` the dialect cannot represent, each value the
#'   reason.
#'
#' @return `sqlr_test_types()` returns `NULL` invisibly;
#'   `sqlr_conformance_types()` returns a named list of [sqlr_type]s.
#'
#' @examples
#' names(sqlr_conformance_types())
#'
#' # in tests/testthat/ of a dialect package, e.g.
#' # sqlr_test_types(
#' #   function() DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
#' #   skip = c(time_tz = "SQLite has no time zone-aware time type")
#' # )
#'
#' @export
sqlr_test_types <- function(connect, skip = character()) {
  stopifnot(is.function(connect))

  types <- sqlr_conformance_types()
  check_skip(skip, names(types))

  for (name in names(types)) {
    testthat::test_that(paste0("sqlr type round trip: ", name), {
      if (name %in% names(skip)) {
        testthat::skip(paste0(name, ": ", skip[[name]]))
      }

      con <- connect()
      on.exit(DBI::dbDisconnect(con))

      differences <- round_trip_type(con, name, types[[name]])
      testthat::expect(
        length(differences) == 0L,
        paste(c("Written vs reflected:", differences), collapse = "\n")
      )
    })
  }

  invisible(NULL)
}

#' @rdname sqlr_test_types
#' @export
sqlr_conformance_types <- function() {
  list(
    tinyint = sqlr_integer_type(bytes = 1L),
    smallint = sqlr_smallint(),
    integer = sqlr_int(),
    bigint = sqlr_bigint(),
    integer_unsigned = sqlr_int(unsigned = TRUE),
    real = sqlr_real(),
    double = sqlr_double(),
    numeric = sqlr_numeric(),
    numeric_precision = sqlr_numeric(10),
    numeric_scale = sqlr_numeric(10, 2),
    char = sqlr_char(3),
    char_unsized = sqlr_char(),
    varchar = sqlr_varchar(255),
    text = sqlr_text(),
    blob = sqlr_blob(),
    varbinary = sqlr_blob(16),
    binary = sqlr_binary_type(size = 16L, fixed = TRUE),
    boolean = sqlr_boolean(),
    date = sqlr_date(),
    time = sqlr_time(),
    time_precision = sqlr_time(precision = 3),
    time_tz = sqlr_time(with_timezone = TRUE),
    timestamp = sqlr_timestamp(),
    timestamp_precision = sqlr_timestamp(precision = 3),
    timestamp_tz = sqlr_timestamp(with_timezone = TRUE),
    json = sqlr_json(),
    json_binary = sqlr_json(binary = TRUE),
    uuid = sqlr_uuid()
  )
}

check_skip <- function(skip, types) {
  named <- length(names(skip)) == length(skip) && all(nzchar(names(skip)))
  reasoned <- is.character(skip) && !anyNA(skip) && all(nzchar(skip))
  if (!named || !reasoned) {
    stop("`skip` must name each type and give a reason for it", call. = FALSE)
  }

  unknown <- setdiff(names(skip), types)
  if (length(unknown)) {
    stop(
      "`skip` names types sqlr_conformance_types() does not have: ",
      paste0(unknown, collapse = ", "),
      call. = FALSE
    )
  }
}

round_trip_type <- function(con, name, type) {
  dialect <- sqlr_for(con)
  table <- sqlr_table(
    paste0("sqlr_", name, "_", Sys.getpid()),
    sqlr_column("x", type)
  )

  on.exit(DBI::dbRemoveTable(con, table@name, fail_if_missing = FALSE))
  for (stmt in sqlr_render(table, dialect)) {
    DBI::dbExecute(con, stmt)
  }

  reflected <- schema_table(sqlr_reflect(con, dialect = dialect), table@name)
  if (is.null(reflected)) {
    return(paste0("table ", table@name, " was not reflected"))
  }

  sqlr_diff(table, reflected)
}
