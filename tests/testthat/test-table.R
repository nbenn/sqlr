test_that("primary key columns are implied NOT NULL", {
  tbl <- sqlr_table(
    "t",
    sqlr_column("id", sqlr_bigint()),
    sqlr_column("other", sqlr_int()),
    sqlr_primary_key("id")
  )

  expect_false(tbl@columns[[1L]]@null)
  expect_true(tbl@columns[[2L]]@null)
})

test_that("duplicate column names are rejected", {
  expect_error(
    sqlr_table("t", sqlr_column("id", sqlr_int()), sqlr_column("id", sqlr_int())),
    "must be unique"
  )
})

test_that("constraints may not name unknown columns", {
  expect_error(
    sqlr_table("t", sqlr_column("id", sqlr_int()), sqlr_primary_key("nope")),
    "unknown columns"
  )
})

test_that("foreign key columns must pair up", {
  expect_error(
    sqlr_foreign_key(c("a", "b"), "other", "id"),
    "same length"
  )
})

test_that("referential actions are checked", {
  expect_error(sqlr_foreign_key("a", "t", "id", on_delete = "explode"), "must be one of")
  expect_equal(sqlr_foreign_key("a", "t", "id", on_delete = "CASCADE")@on_delete, "cascade")
})
