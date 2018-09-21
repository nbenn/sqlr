context("determine mysql column spec from r object")

set_con(section = "mysql_unittest")

test_that("integer column types can be specified", {
  expect_s4_class(get_col_spec(1L, name = "foo"),
                  "SQL")
  expect_equal(as.character(get_col_spec(1L, name = "foo")),
               "`foo` TINYINT UNSIGNED")
  expect_equal(as.character(get_col_spec(1L, name = "foo", key = "primary")),
               "`foo` TINYINT UNSIGNED PRIMARY KEY")
  expect_equal(as.character(get_col_spec(bit64::as.integer64(1L),
                                         name = "foo")),
               "`foo` TINYINT UNSIGNED")
  expect_equal(as.character(get_col_spec(c(bit64::as.integer64(0),
                                           bit64::as.integer64(4294967295)),
                                         name = "foo")),
               "`foo` INT UNSIGNED")
  expect_warning(get_col_spec(1L, name = "foo", unsigned = FALSE))
  expect_error(get_col_spec(1L, name = "foo", min = -1L))
  expect_error(get_col_spec(1L, name = ""))
})

test_that("numeric column types can be specified", {
  expect_s4_class(get_col_spec(1.5, name = "foo"),
                  "SQL")
  expect_equal(as.character(get_col_spec(1.5, name = "foo")),
               "`foo` DOUBLE")
  expect_equal(as.character(get_col_spec(1.5, name = "foo", unsigned = TRUE)),
               "`foo` DOUBLE UNSIGNED")
  expect_equal(as.character(get_col_spec(1.5, name = "foo", nullable = FALSE)),
               "`foo` DOUBLE NOT NULL")
  expect_equal(as.character(get_col_spec(single(1.5), name = "foo")),
               "`foo` FLOAT")
  expect_equal(as.character(get_col_spec(1, name = "foo")),
               "`foo` DOUBLE")
})

test_that("character column types can be specified", {
  expect_s4_class(get_col_spec("a", name = "foo"),
                  "SQL")
  expect_equal(as.character(get_col_spec("a", name = "foo")),
               "`foo` CHAR(1)")
  expect_equal(as.character(get_col_spec(c("a", "ab"), name = "foo")),
               "`foo` VARCHAR(2)")
  expect_equal(as.character(get_col_spec(c("a", "ab", NA), name = "foo")),
               "`foo` VARCHAR(2)")
  expect_equal(as.character(get_col_spec(paste(rep("a", 300), collapse = ""),
                                         name = "foo")),
               "`foo` VARCHAR(300)")
  expect_equal(as.character(get_col_spec(paste(rep("a", 30000), collapse = ""),
                                         name = "foo")),
               "`foo` TEXT")
  expect_equal(as.character(get_col_spec(paste(rep("\U2661", 30000),
                                               collapse = ""),
                                         name = "foo")),
               "`foo` MEDIUMTEXT")
  expect_equal(as.character(get_col_spec(c("a", "\U2661"), name = "foo")),
               "`foo` VARCHAR(3)")
  expect_equal(as.character(get_col_spec("a", name = "foo",
                                         char_set = "ascii")),
               "`foo` CHAR(1) CHARACTER SET 'ascii'")
})

test_that("raw column types can be specified", {
  x <- list(as.raw(40), charToRaw("A"))
  y <- list(as.raw(40), charToRaw(paste(letters, collapse = "")))
  z <- list(charToRaw(paste(rep("a", 30000), collapse = "")))
  expect_s4_class(get_col_spec(x, name = "foo"),
                  "SQL")
  expect_equal(as.character(get_col_spec(x, name = "foo")),
               "`foo` BINARY(48)")
  expect_equal(get_col_spec(x, name = "foo"),
               get_col_spec(blob::as.blob(x), name = "foo"))
  expect_equal(as.character(get_col_spec(y, name = "foo")),
               "`foo` VARBINARY(72)")
  expect_equal(as.character(get_col_spec(y, name = "foo", force_blob = TRUE)),
               "`foo` TINYBLOB")
  expect_equal(as.character(get_col_spec(z, name = "foo")),
               "`foo` BLOB")
  expect_error(get_col_spec(list()))
  expect_error(get_col_spec(list(a = 1)))
  expect_error(get_col_spec(c(x, 1)))
})

test_that("logical column types can be specified", {
  expect_s4_class(get_col_spec(TRUE, name = "foo"),
                  "SQL")
  expect_equal(as.character(get_col_spec(TRUE, name = "foo")),
               "`foo` BOOL")
  expect_equal(as.character(get_col_spec(c(TRUE, NA, FALSE), name = "foo",
                                         key = "primary")),
               "`foo` BOOL PRIMARY KEY")
})

test_that("factor column types can be specified", {
  expect_s4_class(get_col_spec(factor(letters), name = "foo"),
                  "SQL")
  expect_equal(as.character(get_col_spec(factor(letters[1:3]), name = "foo")),
               "`foo` ENUM('a', 'b', 'c')")
  expect_equal(as.character(get_col_spec(factor(letters[1:3]), name = "foo",
                                         char_set = "ascii")),
               "`foo` ENUM('a', 'b', 'c') CHARACTER SET 'ascii'")
  expect_equal(as.character(get_col_spec(factor(letters[1:3]), name = "foo",
                                         variant = "set")),
               "`foo` SET('a', 'b', 'c')")
})

test_that("date/tome column types can be specified", {
  expect_s4_class(get_col_spec(as.POSIXct(Sys.time()), name = "foo"),
                  "SQL")
  expect_equal(as.character(get_col_spec(as.POSIXct(Sys.time()),
                                         name = "foo")),
               "`foo` DATETIME")
  expect_equal(as.character(get_col_spec(as.POSIXlt(Sys.time()),
                                         name = "foo")),
               "`foo` DATETIME")
  expect_equal(as.character(get_col_spec(Sys.Date(), name = "foo")),
               "`foo` DATE")
  expect_equal(as.character(get_col_spec(difftime(Sys.time(), Sys.Date()),
                                         name = "foo")),
               "`foo` TIME")
  expect_equal(as.character(get_col_spec(hms::as.hms(12), name = "foo")),
               "`foo` TIME")
})

test_that("column types can be specified for a data.frame", {
  x <- data.frame(a = 1:3, b = letters[1:3])
  expect_true(all(sapply(get_col_spec(x), inherits, "SQL")))
  expect_type(get_col_spec(x), "list")
  expect_named(get_col_spec(x), c("a", "b"))
  expect_equal(as.character(get_col_spec(x)),
               c("`a` TINYINT UNSIGNED", "`b` ENUM('a', 'b', 'c')"))
  expect_equal(as.character(get_col_spec(x, nullable = FALSE)),
               c("`a` TINYINT UNSIGNED NOT NULL",
                 "`b` ENUM('a', 'b', 'c') NOT NULL"))
  expect_equal(as.character(get_col_spec(x, nullable = c(TRUE, FALSE))),
               c("`a` TINYINT UNSIGNED",
                 "`b` ENUM('a', 'b', 'c') NOT NULL"))
  expect_equal(as.character(get_col_spec(x, nullable = list(TRUE, FALSE))),
               c("`a` TINYINT UNSIGNED",
                 "`b` ENUM('a', 'b', 'c') NOT NULL"))
  expect_error(get_col_spec(x, nullable = rep(TRUE, 3)))
  expect_equal(as.character(get_col_spec(x,
                                         nullable = c(TRUE, FALSE),
                                         default = list(1, "a"),
                                         comment = "foo")),
               c("`a` TINYINT UNSIGNED DEFAULT 1 COMMENT 'foo'",
                 "`b` ENUM('a', 'b', 'c') NOT NULL DEFAULT 'a' COMMENT 'foo'"))
  expect_equal(as.character(get_col_spec(tibble::as_tibble(x))),
               c("`a` TINYINT UNSIGNED", "`b` ENUM('a', 'b', 'c')"))
})

rm_con()