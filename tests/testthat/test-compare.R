test_that("constraint order and naming do not affect equality", {
  a <- sqlr_table(
    "t",
    sqlr_column("id", sqlr_int()),
    sqlr_column("email", sqlr_text()),
    sqlr_primary_key("id", name = "t_pkey"),
    sqlr_unique("email", name = "t_email_key")
  )
  b <- sqlr_table(
    "t",
    sqlr_column("id", sqlr_int()),
    sqlr_column("email", sqlr_text()),
    sqlr_unique("email", name = "uq_t_email"),
    sqlr_primary_key("id", name = "pk_t")
  )

  expect_true(sqlr_equal(a, b))
})

test_that("the reflected type spelling is ignored", {
  a <- sqlr_table("t", sqlr_column("id", sqlr_int()))
  b <- sqlr_table("t", sqlr_column("id", sqlr_int(raw = "int4")))

  expect_true(sqlr_equal(a, b))
})

test_that("differences are reported", {
  a <- sqlr_table("t", sqlr_column("id", sqlr_int()))
  b <- sqlr_table("t", sqlr_column("id", sqlr_bigint()))

  expect_match(sqlr_diff(a, b), "type")

  c <- sqlr_table("t", sqlr_column("id", sqlr_int(), null = FALSE))
  expect_match(sqlr_diff(a, c), "null")
})

test_that("a type difference is reported in SQL spelling", {
  a <- sqlr_table("t", sqlr_column("x", "varchar(255)"))
  b <- sqlr_table("t", sqlr_column("x", sqlr_text()))

  expect_equal(sqlr_diff(a, b), "column x: type varchar(255) vs text")
})

test_that("missing and extra tables are reported", {
  a <- sqlr_schema("s", sqlr_table("x", sqlr_column("id", sqlr_int())))
  b <- sqlr_schema("s", sqlr_table("y", sqlr_column("id", sqlr_int())))

  expect_match(sqlr_diff(a, b), "missing tables: x", all = FALSE)
  expect_match(sqlr_diff(a, b), "extra tables: y", all = FALSE)
})
