context("determine mysql column spec from r object")

mysql <- sqlr:::get_con(section = "mysql_unittest")

test_that("integer column types can be specified", {
  expect_s4_class(get_col_spec(1L, name = "foo", con = mysql),
                  "SQL")
  expect_equal(as.character(get_col_spec(1L, name = "foo", con = mysql)),
               "`foo` TINYINT UNSIGNED")
  expect_equal(as.character(get_col_spec(1L, name = "foo", key = "primary",
                                         con = mysql)),
               "`foo` TINYINT UNSIGNED PRIMARY KEY")
  expect_equal(as.character(get_col_spec(bit64::as.integer64(1L),
                                         name = "foo", con = mysql)),
               "`foo` TINYINT UNSIGNED")
  expect_equal(as.character(get_col_spec(c(bit64::as.integer64(0),
                                           bit64::as.integer64(4294967295)),
                                         name = "foo", con = mysql)),
               "`foo` INT UNSIGNED")
  expect_warning(get_col_spec(1L, name = "foo", unsigned = FALSE, con = mysql))
  expect_error(get_col_spec(1L, name = "foo", min = -1L, con = mysql))
  expect_error(get_col_spec(1L, name = "", con = mysql))
})

test_that("numeric column types can be specified", {
  expect_s4_class(get_col_spec(1.5, name = "foo", con = mysql),
                  "SQL")
  expect_equal(as.character(get_col_spec(1.5, name = "foo", con = mysql)),
               "`foo` DOUBLE")
  expect_equal(as.character(get_col_spec(1.5, name = "foo", unsigned = TRUE,
                                         con = mysql)),
               "`foo` DOUBLE UNSIGNED")
  expect_equal(as.character(get_col_spec(1.5, name = "foo", nullable = FALSE,
                                         con = mysql)),
               "`foo` DOUBLE NOT NULL")
  expect_equal(as.character(get_col_spec(single(1.5), name = "foo",
                                         con = mysql)),
               "`foo` FLOAT")
  expect_equal(as.character(get_col_spec(1, name = "foo", con = mysql)),
               "`foo` DOUBLE")
})

test_that("character column types can be specified", {
  expect_s4_class(get_col_spec("a", name = "foo", con = mysql),
                  "SQL")
  expect_equal(as.character(get_col_spec("a", name = "foo", con = mysql)),
               "`foo` CHAR(1)")
  expect_equal(as.character(get_col_spec(c("a", "ab"), name = "foo",
                                         con = mysql)),
               "`foo` VARCHAR(2)")
  expect_equal(as.character(get_col_spec(c("a", "ab", NA), name = "foo",
                                         con = mysql)),
               "`foo` VARCHAR(2)")
  expect_equal(as.character(get_col_spec(paste(rep("a", 300), collapse = ""),
                                         name = "foo", con = mysql)),
               "`foo` VARCHAR(300)")
  expect_equal(as.character(get_col_spec(paste(rep("a", 30000), collapse = ""),
                                         name = "foo", con = mysql)),
               "`foo` TEXT")
  expect_equal(as.character(get_col_spec(paste(rep("\U2661", 30000),
                                               collapse = ""),
                                         name = "foo", con = mysql)),
               "`foo` MEDIUMTEXT")
  expect_equal(as.character(get_col_spec(c("a", "\U2661"), name = "foo",
                                         con = mysql)),
               "`foo` VARCHAR(3)")
  expect_equal(as.character(get_col_spec("a", name = "foo", char_set = "ascii",
                                         con = mysql)),
               "`foo` CHAR(1) CHARACTER SET 'ascii'")
})

test_that("raw column types can be specified", {
  x <- list(as.raw(40), charToRaw("A"))
  y <- list(as.raw(40), charToRaw(paste(letters, collapse = "")))
  z <- list(charToRaw(paste(rep("a", 30000), collapse = "")))
  expect_s4_class(get_col_spec(x, name = "foo", con = mysql),
                  "SQL")
  expect_equal(as.character(get_col_spec(x, name = "foo", con = mysql)),
               "`foo` BINARY(48)")
  expect_equal(get_col_spec(x, name = "foo", con = mysql),
               get_col_spec(blob::as.blob(x), name = "foo"))
  expect_equal(as.character(get_col_spec(y, name = "foo", con = mysql)),
               "`foo` VARBINARY(72)")
  expect_equal(as.character(get_col_spec(y, name = "foo", force_blob = TRUE,
                                         con = mysql)),
               "`foo` TINYBLOB")
  expect_equal(as.character(get_col_spec(z, name = "foo", con = mysql)),
               "`foo` BLOB")
  expect_error(get_col_spec(list()))
  expect_error(get_col_spec(list(a = 1)))
  expect_error(get_col_spec(c(x, 1)))
})

test_that("logical column types can be specified", {
  expect_s4_class(get_col_spec(TRUE, name = "foo", con = mysql),
                  "SQL")
  expect_equal(as.character(get_col_spec(TRUE, name = "foo", con = mysql)),
               "`foo` BOOL")
  expect_equal(as.character(get_col_spec(c(TRUE, NA, FALSE), name = "foo",
                                         key = "primary", con = mysql)),
               "`foo` BOOL PRIMARY KEY")
})

test_that("factor column types can be specified", {
  expect_s4_class(get_col_spec(factor(letters), name = "foo", con = mysql),
                  "SQL")
  expect_equal(as.character(get_col_spec(factor(letters[1:3]), name = "foo",
                                         con = mysql)),
               "`foo` ENUM('a', 'b', 'c')")
  expect_equal(as.character(get_col_spec(factor(letters[1:3]), name = "foo",
                                         char_set = "ascii", con = mysql)),
               "`foo` ENUM('a', 'b', 'c') CHARACTER SET 'ascii'")
  expect_equal(as.character(get_col_spec(factor(letters[1:3]), name = "foo",
                                         variant = "set", con = mysql)),
               "`foo` SET('a', 'b', 'c')")
})

test_that("date/tome column types can be specified", {
  expect_s4_class(get_col_spec(as.POSIXct(Sys.time()), name = "foo",
                               con = mysql),
                  "SQL")
  expect_equal(as.character(get_col_spec(as.POSIXct(Sys.time()), name = "foo",
                                         con = mysql)),
               "`foo` DATETIME")
  expect_equal(as.character(get_col_spec(as.POSIXlt(Sys.time()), name = "foo",
                                         con = mysql)),
               "`foo` DATETIME")
  expect_equal(as.character(get_col_spec(Sys.Date(), name = "foo",
                                         con = mysql)),
               "`foo` DATE")
  expect_equal(as.character(get_col_spec(difftime(Sys.time(), Sys.Date()),
                                         name = "foo", con = mysql)),
               "`foo` TIME")
  expect_equal(as.character(get_col_spec(hms::as.hms(12),
                                         name = "foo", con = mysql)),
               "`foo` TIME")
})

rm_con()