test_that("type shorthand parses standard spellings", {
  expect_equal(as_sqlr_type("varchar(255)")@size, 255L)
  expect_true(as_sqlr_type("char(3)")@fixed)
  expect_equal(as_sqlr_type("numeric(10, 2)")@scale, 2L)
  expect_equal(as_sqlr_type("bigint")@bytes, 8L)
  expect_equal(as_sqlr_type("int4")@bytes, 4L)
  expect_true(as_sqlr_type("timestamptz")@with_timezone)
  expect_true(as_sqlr_type("jsonb")@binary)
})

test_that("unknown spellings survive as sqlr_other", {
  type <- as_sqlr_type("geometry")

  expect_s3_class(type, "sqlr::sqlr_other_type")
  expect_equal(type@name, "geometry")
  expect_equal(type@raw, "geometry")
})

test_that("a type passes through unchanged", {
  type <- sqlr_bigint()

  expect_identical(as_sqlr_type(type), type)
})

test_that("integer widths are constrained", {
  expect_error(sqlr_integer_type(bytes = 3L), "1, 2, 4 or 8")
})
