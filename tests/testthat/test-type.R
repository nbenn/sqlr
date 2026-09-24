test_that("each SQL-standard spelling parses to the type it names", {
  spellings <- list(
    "smallint" = sqlr_smallint(),
    "integer" = sqlr_int(),
    "int" = sqlr_int(),
    "bigint" = sqlr_bigint(),
    "real" = sqlr_real(),
    "double precision" = sqlr_double(),
    "float(24)" = sqlr_real(),
    "float(25)" = sqlr_double(),
    "numeric" = sqlr_numeric(),
    "numeric(10)" = sqlr_numeric(10),
    "decimal(10, 2)" = sqlr_numeric(10, 2),
    "char" = sqlr_char(1),
    "character(3)" = sqlr_char(3),
    "varchar(255)" = sqlr_varchar(255),
    "character varying(255)" = sqlr_varchar(255),
    "text" = sqlr_text(),
    "binary(16)" = sqlr_binary_type(size = 16L, fixed = TRUE),
    "varbinary(16)" = sqlr_blob(16),
    "blob" = sqlr_blob(),
    "boolean" = sqlr_boolean(),
    "date" = sqlr_date(),
    "time" = sqlr_time(),
    "time(3) without time zone" = sqlr_time(precision = 3),
    "time with time zone" = sqlr_time(with_timezone = TRUE),
    "timestamp without time zone" = sqlr_timestamp(),
    "timestamp(3) with time zone" = sqlr_timestamp(TRUE, 3),
    "json" = sqlr_json(),
    "uuid" = sqlr_uuid()
  )

  expect_equal(
    sapply(names(spellings), as_sqlr_type, simplify = FALSE),
    spellings
  )
})

test_that("case and spacing do not matter", {
  expect_equal(as_sqlr_type(" Numeric( 10 ,2 ) "), sqlr_numeric(10, 2))
  expect_equal(as_sqlr_type("DOUBLE\tPRECISION"), sqlr_double())
  expect_equal(
    as_sqlr_type("TIMESTAMP (3)WITH  TIME ZONE"),
    sqlr_timestamp(TRUE, 3)
  )
})

test_that("engine aliases, typos and ambiguous spellings are errors", {
  refused <- c(
    "int4", "timestamptz", "bytea", "double", "jsonb", "float", "float(54)",
    "varchar", "integer(10)", "varchr(255)", "geometry(Point, 4326)"
  )

  for (x in refused) {
    expect_error(as_sqlr_type(x), "unrecognised type", info = x)
  }
  expect_error(as_sqlr_type("int4"), "sqlr_other(\"int4\")", fixed = TRUE)
})

test_that("a type formats as its standard spelling", {
  spellings <- c(
    "smallint", "integer", "bigint", "real", "double precision", "numeric",
    "numeric(10)", "numeric(10, 2)", "char(3)", "varchar(255)", "text",
    "binary(16)", "varbinary(16)", "blob", "boolean", "date", "time(3)",
    "time with time zone", "timestamp", "timestamp(3) with time zone", "json",
    "uuid"
  )

  formatted <- vapply(
    spellings,
    function(x) format(as_sqlr_type(x)),
    character(1L),
    USE.NAMES = FALSE
  )
  expect_equal(formatted, spellings)
})

test_that("a type without a standard spelling formats as a constructor call", {
  types <- list(
    sqlr_integer_type(bytes = 1L),
    sqlr_int(unsigned = TRUE),
    sqlr_char(),
    sqlr_json(binary = TRUE),
    sqlr_other("geometry")
  )
  calls <- vapply(types, format, character(1L))

  expect_equal(calls[[1L]], "sqlr_integer_type(bytes = 1L)")
  expect_equal(lapply(calls, function(x) eval(parse(text = x))), types)
})

test_that("a type passes through unchanged", {
  type <- sqlr_bigint()

  expect_identical(as_sqlr_type(type), type)
})

test_that("integer widths are constrained", {
  expect_error(sqlr_integer_type(bytes = 3L), "1, 2, 4 or 8")
})
