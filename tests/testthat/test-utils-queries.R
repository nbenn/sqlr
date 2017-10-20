context("utils: test query utils")

set_con(section = "mysql_unittest")
drop_db_tbl(c("plane"), force = TRUE)
write_db_tbl("plane", planes, keys = pk_spec("tailnum"), char_set = "ascii")

test_that("columns can be listed", {
  expect_error(show_db_cols(c("plane", "airport")))
  expect_error(show_db_cols("plane", full = "year"))
  expect_error(show_db_cols("plane", parse = 1L))
  expect_error(show_db_cols("plane", like = ""))
  expect_error(show_db_cols("plane", like = c("year", "type")))
  expect_error(show_db_cols("plane", where = "year"))
  expect_is(show_db_cols("plane"), "data.frame")
  expect_is(show_db_cols("plane"), "tbl_df")
  expect_equal(ncol(show_db_cols("plane")), 6L)
  expect_named(show_db_cols("plane"), c("Field", "Type", "Null", "Key",
                                        "Default", "Extra"))
  expect_equal(nrow(show_db_cols("plane")), 9L)
  expect_equal(nrow(show_db_cols("plane", like = "year")), 1L)
  expect_equal(nrow(show_db_cols("plane", where = DBI::SQL("Field = 'year'"))),
               1L)
  expect_equal(ncol(show_db_cols("plane", full = TRUE)), 9L)
  expect_named(show_db_cols("plane", full = TRUE),
               c("Field", "Type", "Collation", "Null", "Key", "Default",
                 "Extra", "Privileges", "Comment"))
  expect_equal(ncol(show_db_cols("plane", parse = TRUE)), 8L)
  expect_named(show_db_cols("plane", parse = TRUE),
               c("Field", "Type", "Length", "Unsigned", "Null", "Key",
                 "Default", "Extra"))
  expect_equal(ncol(show_db_cols("plane", full = TRUE, parse = TRUE)), 12L)
  expect_named(show_db_cols("plane", full = TRUE, parse = TRUE),
               c("Field", "Type", "Length", "Unsigned", "Charset",
                 "Collation", "Null", "Key", "Default", "Extra", "Privileges",
                 "Comment"))
  expect_is(show_db_cols("plane", parse = TRUE)$Unsigned, "logical")
  expect_is(show_db_cols("plane", parse = TRUE)$Null, "logical")
})

drop_db_tbl(c("plane"), force = TRUE)
rm_con()
