context("mysql column specification")

test_that("column types can be specified", {
  foo <- "bar"
  expect_is(
    col_spec(),
    "sqlr_col_spec"
  )
  expect_render(
    col_spec(name = "foo"),
    "`foo` INT"
  )
  expect_render2(
    col_spec(name = "foo", type = col_int()),
    col_spec(name = "foo", type = col_int)
  )
  expect_render2(
    col_spec(name = "foo", type = col_int()),
    col_spec(name = "foo", type = "col_int")
  )
  expect_render2(
    col_spec(name = "foo", type = col_int()),
    col_spec(name = "foo", type = "int")
  )
  expect_error(col_spec(type = "foo"))
  expect_error(col_spec(type = foo))
  expect_error(col_spec(type = mean))
  expect_error_render(col_spec(name = ""))
  expect_render(
    col_spec(
      name = "foo", type = "int",
      unsigned = TRUE
    ),
    "`foo` INT UNSIGNED"
  )
  expect_error(col_spec(key = "foo"))
  expect_error_render(col_spec(auto_increment = "yes"))
  expect_render(
    col_spec(
      name = "foo", nullable = FALSE,
      default = "bar", auto_increment = TRUE,
      key = "key", comment = "fobar"
    ),
    paste("`foo` INT NOT NULL DEFAULT 'bar' AUTO_INCREMENT KEY",
      "COMMENT 'fobar'",
      collapse = ""
    )
  )
  expect_render(
    col_spec(
      name = "foo", auto_increment = TRUE,
      key = "primary"
    ),
    "`foo` INT AUTO_INCREMENT PRIMARY KEY"
  )
  expect_error_render(col_spec(name = "foo", comment = ""))
  expect_render(
    col_spec(name = "fo.o"),
    "`fo.o` INT"
  )
  expect_render(
    col_spec(name = "fo'o"),
    "`fo'o` INT"
  )
  expect_render(
    col_spec(name = "bar", default = "fo'o"),
    "`bar` INT DEFAULT 'fo\\'o'"
  )
  expect_render(
    col_spec(name = "bar", default = "foo\n"),
    "`bar` INT DEFAULT 'foo\\n'"
  )
  expect_render(
    col_spec(name = "foo", default = 1L),
    "`foo` INT DEFAULT 1"
  )
  expect_render(
    col_spec(name = "foo", default = 1.5),
    "`foo` INT DEFAULT 1.5"
  )
  expect_render(
    col_spec(name = "foo", default = "bar"),
    "`foo` INT DEFAULT 'bar'"
  )
  expect_render(
    col_spec(name = "foo", default = ""),
    "`foo` INT DEFAULT ''"
  )
  expect_render(
    col_spec(name = "foo", default = NULL),
    "`foo` INT"
  )
  expect_render(
    col_spec(name = "foo", default = NA),
    "`foo` INT DEFAULT NULL"
  )
  expect_render(
    col_spec(name = "foo", default = NA_character_),
    "`foo` INT DEFAULT NULL"
  )
  expect_render(
    col_spec(name = "foo", default = FALSE),
    "`foo` INT DEFAULT 0"
  )
})

test_that("integer data types can be specified", {
  expect_is(
    col_int(),
    "sqlr_col_int"
  )
  expect_error(col_int(size = "foo"))
  expect_error(col_int(unsigned = "foo"))
  expect_error(col_int(min = "foo"))
  expect_warning(
    col_int(unsigned = TRUE, min = -1L),
    "param \"unsigned\" will be ignored."
  )
  expect_warning(
    col_int(size = "tiny", max = 1L),
    "param \"size\" will be ignored."
  )
  expect_render2(
    col_int(min = -1L, max = 1L),
    col_int(size = "tiny")
  )
  expect_render2(
    col_int(min = -128L, max = 127L),
    col_int(size = "tiny")
  )
  expect_render2(
    col_int(min = 0L, max = 255L),
    col_int(size = "tiny", unsigned = TRUE)
  )
  expect_render2(
    col_int(min = 0L, max = 256L),
    col_int(size = "small", unsigned = TRUE)
  )
  expect_render2(
    col_int(min = 0L, max = 65535L),
    col_int(size = "small", unsigned = TRUE)
  )
  expect_render2(
    col_int(min = -1L, max = 65535L),
    col_int(size = "medium")
  )
  expect_render2(
    col_int(min = 0L, max = 16777215L),
    col_int(size = "medium", unsigned = TRUE)
  )
  expect_render2(
    col_int(
      min = bit64::as.integer64(-2147483648),
      max = 2147483647L
    ),
    col_int()
  )
  expect_render2(
    col_int(
      min = 0L,
      max = bit64::as.integer64(4294967295)
    ),
    col_int(unsigned = TRUE)
  )
  expect_render2(
    col_int(
      min = 0L,
      max = bit64::as.integer64(4294967296)
    ),
    col_int(size = "big", unsigned = TRUE)
  )
  expect_render2(
    col_int(
      min = -1L,
      max = bit64::as.integer64(4294967296)
    ),
    col_int(size = "big")
  )
})

test_that("floating point data types can be specified", {
  expect_is(
    col_dbl(),
    "sqlr_col_dbl"
  )
  expect_error_render(col_dbl(prec = "foo"))
  expect_error_render(col_dbl(unsigned = 1))
  expect_render(
    col_dbl(),
    "DOUBLE"
  )
  expect_render(
    col_dbl(prec = "single"),
    "FLOAT"
  )
  expect_render(
    col_dbl(prec = "double", unsigned = TRUE),
    "DOUBLE UNSIGNED"
  )
})

test_that("string data types can be specified", {
  expect_is(
    col_chr(),
    "sqlr_col_chr"
  )
  expect_error_render(col_chr(length = "foo"))
  expect_error_render(col_chr(length = -1L))
  expect_error_render(col_chr(length = 256, fixed = TRUE))
  expect_error_render(col_chr(fixed = TRUE, force_text = TRUE))
  expect_render(
    col_chr(),
    "VARCHAR(255)"
  )
  expect_render(
    col_chr(fixed = TRUE),
    "CHAR(255)"
  )
  expect_render(
    col_chr(force_text = TRUE),
    "TINYTEXT"
  )
  expect_render(
    col_chr(length = 16383L),
    "VARCHAR(16383)"
  )
  expect_render(
    col_chr(length = 16384L),
    "TEXT"
  )
  expect_warning_render(
    col_chr(length = 16384L, force_text = FALSE),
    "possibly consider TEXT instead"
  )
  expect_render(
    col_chr(length = 65535L),
    "TEXT"
  )
  expect_render(
    col_chr(length = 65536L),
    "MEDIUMTEXT"
  )
  expect_render(
    col_chr(length = 16777215L),
    "MEDIUMTEXT"
  )
  expect_render(
    col_chr(length = 16777216L),
    "LONGTEXT"
  )
  expect_render(
    col_chr(char_set = "foo", collate = "bar"),
    "VARCHAR(255) CHARACTER SET 'foo' COLLATE 'bar'"
  )
  expect_render(
    col_chr(char_set = "foo"),
    "VARCHAR(255) CHARACTER SET 'foo'"
  )
  expect_render(
    col_chr(collate = "bar"),
    "VARCHAR(255) COLLATE 'bar'"
  )
  expect_render(
    col_chr(char_set = "fo'o"),
    "VARCHAR(255) CHARACTER SET 'fo\\'o'"
  )
  expect_render(
    col_chr(char_set = "foo\n"),
    "VARCHAR(255) CHARACTER SET 'foo\\n'"
  )
  expect_error_render(col_chr(char_set = ""))
})

test_that("binary data types can be specified", {
  expect_is(
    col_raw(),
    "sqlr_col_raw"
  )
  expect_error_render(col_raw(length = "foo"))
  expect_error_render(col_raw(length = -1L))
  expect_error_render(col_raw(length = 256, fixed = TRUE))
  expect_error_render(col_raw(fixed = TRUE, force_blob = TRUE))
  expect_render(
    col_raw(),
    "VARBINARY(255)"
  )
  expect_render(
    col_raw(fixed = TRUE),
    "BINARY(255)"
  )
  expect_render(
    col_raw(force_blob = TRUE),
    "TINYBLOB"
  )
  expect_render(
    col_raw(length = 16383L),
    "VARBINARY(16383)"
  )
  expect_render(
    col_raw(length = 16384L),
    "BLOB"
  )
  expect_warning_render(
    col_raw(length = 16384L, force_blob = FALSE),
    "possibly consider BLOB instead"
  )
  expect_render(
    col_raw(length = 65535L),
    "BLOB"
  )
  expect_render(
    col_raw(length = 65536L),
    "MEDIUMBLOB"
  )
  expect_render(
    col_raw(length = 16777215L),
    "MEDIUMBLOB"
  )
  expect_render(
    col_raw(length = 16777216L),
    "LONGBLOB"
  )
})

test_that("boolean data types can be specified", {
  expect_is(
    col_lgl(),
    "sqlr_col_lgl"
  )
  expect_render(
    col_lgl(),
    "BOOL"
  )
})

test_that("enum data types can be specified", {
  expect_is(
    col_fct(levels = letters[1:3]),
    "sqlr_col_fct"
  )
  expect_error_render(col_fct())
  expect_error(col_fct(levels = letters[1:3], variant = "foo"))
  expect_render(
    col_fct(levels = letters[1:3]),
    "ENUM('a', 'b', 'c')"
  )
  expect_render(
    col_fct(levels = letters[c(1:3, 3)]),
    "ENUM('a', 'b', 'c')"
  )
  expect_render(
    col_fct(levels = letters[1:3], variant = "set"),
    "SET('a', 'b', 'c')"
  )
  expect_error_render(col_fct(levels = NA_character_))
  expect_render(
    col_fct(levels = "fo'o"),
    "ENUM('fo\\'o')"
  )
  expect_render(
    col_fct(levels = "foo\n"),
    "ENUM('foo\\n')"
  )
  expect_render(
    col_fct(levels = ""),
    "ENUM('')"
  )
  expect_render(
    col_fct(
      levels = letters[1:3], char_set = "foo",
      collate = "bar"
    ),
    "ENUM('a', 'b', 'c') CHARACTER SET 'foo' COLLATE 'bar'"
  )
  expect_render(
    col_fct(levels = letters[1:3], char_set = "foo"),
    "ENUM('a', 'b', 'c') CHARACTER SET 'foo'"
  )
  expect_render(
    col_fct(levels = letters[1:3], collate = "bar"),
    "ENUM('a', 'b', 'c') COLLATE 'bar'"
  )
  expect_render(
    col_fct(levels = letters[1:3], char_set = "fo'o"),
    "ENUM('a', 'b', 'c') CHARACTER SET 'fo\\'o'"
  )
  expect_render(
    col_fct(
      levels = letters[1:3],
      char_set = "foo\n"
    ),
    "ENUM('a', 'b', 'c') CHARACTER SET 'foo\\n'"
  )
  expect_render(
    col_fct(levels = letters[1:3], char_set = ""),
    "ENUM('a', 'b', 'c')"
  )
})

test_that("date/time data types can be specified", {
  expect_is(
    col_dtm(),
    "sqlr_col_dtm"
  )
  expect_render(
    col_dtm(),
    "DATETIME"
  )
  expect_warning(
    col_dtm(class = "datetime", val = 2001L),
    "param \"class\" will be ignored."
  )
  expect_error(col_dtm(val = "foo"))
  expect_error(col_dtm(val = 1L))
  expect_error(col_dtm(val = 12L))
  expect_error(col_dtm(val = 123L))
  expect_render(
    col_dtm(val = 1234L),
    "YEAR"
  )
  expect_error(col_dtm(val = 12345L))
  expect_error(col_dtm(class = "foo"))
  expect_render(
    col_dtm(val = as.POSIXlt(Sys.time())),
    "DATETIME"
  )
  expect_equal(
    col_dtm(val = as.POSIXct(Sys.time())),
    col_dtm(val = as.POSIXlt(Sys.time()))
  )
  expect_equal(
    col_dtm(val = as.POSIXct(Sys.time())),
    col_dtm(val = as.POSIXlt(Sys.time()))
  )
  expect_render(
    col_dtm(val = Sys.Date()),
    "DATE"
  )
  expect_render(
    col_dtm(val = difftime(Sys.time(), Sys.Date())),
    "TIME"
  )
})

test_that("id column can be specified", {
  expect_is(
    col_id(),
    "sqlr_col_spec"
  )
  expect_equal(
    col_id(),
    col_spec(
      name = "id", type = "int", unsigned = TRUE,
      nullable = FALSE, auto_increment = TRUE,
      key = "primary"
    )
  )
  expect_render(
    col_id(key = NULL),
    "`id` INT UNSIGNED NOT NULL AUTO_INCREMENT"
  )
  expect_render(
    col_id(type = "int", size = "big"),
    "`id` BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY"
  )
  expect_type(col_id(as_lst = TRUE), "list")
  expect_named(col_id(as_lst = TRUE), "id")
  expect_equal(
    col_id(),
    col_id(as_lst = TRUE)[[1]]
  )
})
