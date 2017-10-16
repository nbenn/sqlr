context("mysql column specification")

mysql <- sqlr:::get_con(section = "mysql_unittest")

test_that("column types can be specified", {
  foo <- "bar"
  expect_s4_class(col_spec(con = mysql),
                  "SQL")
  expect_equal(as.character(col_spec(name = "foo", con = mysql)),
               "`foo` INT")
  expect_equal(col_spec(name = "foo", type = col_int(), con = mysql),
               col_spec(name = "foo", type = col_int, con = mysql))
  expect_equal(col_spec(name = "foo", type = col_int(), con = mysql),
               col_spec(name = "foo", type = "col_int", con = mysql))
  expect_equal(col_spec(name = "foo", type = col_int(), con = mysql),
               col_spec(name = "foo", type = "int", con = mysql))
  expect_error(col_spec(type = "foo", con = mysql))
  expect_error(col_spec(type = foo, con = mysql))
  expect_error(col_spec(type = mean, con = mysql))
  expect_error(col_spec(name = "", con = mysql))
  expect_equal(as.character(col_spec(name = "foo", type = "int",
                                     unsigned = TRUE, con = mysql)),
               "`foo` INT UNSIGNED")
  expect_error(col_spec(key = "foo", con = mysql))
  expect_error(col_spec(auto_increment = "yes", con = mysql))
  expect_equal(as.character(col_spec(name = "foo", nullable = FALSE,
                                     default = "bar", auto_increment = TRUE,
                                     key = "key", comment = "fobar",
                                     con = mysql)),
               paste("`foo` INT NOT NULL DEFAULT 'bar' AUTO_INCREMENT KEY",
                     "COMMENT 'fobar'", collapse = ""))
  expect_equal(as.character(col_spec(name = "foo", auto_increment = TRUE,
                                     key = "primary", con = mysql)),
               "`foo` INT AUTO_INCREMENT PRIMARY KEY")
  expect_error(col_spec(name = "foo", comment = "", con = mysql))
  expect_equal(as.character(col_spec(name = "fo.o", con = mysql)),
               "`fo.o` INT")
  expect_equal(as.character(col_spec(name = "fo'o", con = mysql)),
               "`fo'o` INT")
  expect_equal(as.character(col_spec(name = "bar", default = "fo'o",
                                     con = mysql)),
               "`bar` INT DEFAULT 'fo\\'o'")
  expect_equal(as.character(col_spec(name = "bar", default = "foo\n",
                                     con = mysql)),
               "`bar` INT DEFAULT 'foo\\n'")
  expect_equal(as.character(col_spec(name = "foo", default = 1L, con = mysql)),
               "`foo` INT DEFAULT 1")
  expect_equal(as.character(col_spec(name = "foo", default = 1.5,
                                     con = mysql)),
               "`foo` INT DEFAULT 1.5")
  expect_equal(as.character(col_spec(name = "foo", default = "bar",
                                     con = mysql)),
               "`foo` INT DEFAULT 'bar'")
  expect_equal(as.character(col_spec(name = "foo", default = "", con = mysql)),
               "`foo` INT DEFAULT ''")
  expect_equal(as.character(col_spec(name = "foo", default = NULL,
                                     con = mysql)),
               "`foo` INT")
  expect_equal(as.character(col_spec(name = "foo", default = NA,
                                     con = mysql)),
               "`foo` INT DEFAULT NULL")
  expect_equal(as.character(col_spec(name = "foo", default = NA_character_,
                                     con = mysql)),
               "`foo` INT DEFAULT NULL")
  expect_equal(as.character(col_spec(name = "foo", default = FALSE,
                                     con = mysql)),
               "`foo` INT DEFAULT 0")
})

test_that("integer data types can be specified", {
  expect_s4_class(col_int(con = mysql),
                  "SQL")
  expect_error(col_int(type = "foo", con = mysql))
  expect_error(col_int(unsigned = "foo", con = mysql))
  expect_error(col_int(min = "foo", con = mysql))
  expect_warning(col_int(unsigned = TRUE, min = -1L, con = mysql),
                 "param \"unsigned\" will be ignored.")
  expect_warning(col_int(type = "tiny", max = 1L, con = mysql),
                 "param \"type\" will be ignored.")
  expect_equal(col_int(min = -1L, max = 1L, con = mysql),
               col_int(type = "tiny", con = mysql))
  expect_equal(col_int(min = -128L, max = 127L, con = mysql),
               col_int(type = "tiny", con = mysql))
  expect_equal(col_int(min = 0L, max = 255L, con = mysql),
               col_int(type = "tiny", unsigned = TRUE, con = mysql))
  expect_equal(col_int(min = 0L, max = 256L, con = mysql),
               col_int(type = "small", unsigned = TRUE, con = mysql))
  expect_equal(col_int(min = 0L, max = 65535L, con = mysql),
               col_int(type = "small", unsigned = TRUE, con = mysql))
  expect_equal(col_int(min = -1L, max = 65535L, con = mysql),
               col_int(type = "medium", con = mysql))
  expect_equal(col_int(min = 0L, max = 16777215L, con = mysql),
               col_int(type = "medium", unsigned = TRUE, con = mysql))
  expect_equal(col_int(min = bit64::as.integer64(-2147483648),
                       max = 2147483647L, con = mysql),
               col_int(con = mysql))
  expect_equal(col_int(min = 0L,
                       max = bit64::as.integer64(4294967295), con = mysql),
               col_int(unsigned = TRUE, con = mysql))
  expect_equal(col_int(min = 0L,
                       max = bit64::as.integer64(4294967296), con = mysql),
               col_int(type = "big", unsigned = TRUE, con = mysql))
  expect_equal(col_int(min = -1L,
                       max = bit64::as.integer64(4294967296), con = mysql),
               col_int(type = "big", con = mysql))
})

test_that("floating point data types can be specified", {
  expect_s4_class(col_dbl(con = mysql),
                  "SQL")
  expect_error(col_dbl(prec = "foo", con = mysql))
  expect_error(col_dbl(unsigned = 1, con = mysql))
  expect_equal(as.character(col_dbl(con = mysql)),
               "DOUBLE")
  expect_equal(as.character(col_dbl(prec = "single", con = mysql)),
               "FLOAT")
  expect_equal(as.character(col_dbl(prec = "double", unsigned = TRUE,
                                    con = mysql)),
               "DOUBLE UNSIGNED")
})

test_that("string data types can be specified", {
  expect_s4_class(col_chr(con = mysql),
                  "SQL")
  expect_error(col_chr(length = "foo", con = mysql))
  expect_error(col_chr(length = -1L, con = mysql))
  expect_error(col_chr(length = 256, fixed = TRUE, con = mysql))
  expect_error(col_chr(fixed = TRUE, force_text = TRUE, con = mysql))
  expect_equal(as.character(col_chr(con = mysql)),
               "VARCHAR(255)")
  expect_equal(as.character(col_chr(fixed = TRUE, con = mysql)),
               "CHAR(255)")
  expect_equal(as.character(col_chr(force_text = TRUE, con = mysql)),
               "TINYTEXT")
  expect_equal(as.character(col_chr(length = 16383L, con = mysql)),
               "VARCHAR(16383)")
  expect_equal(as.character(col_chr(length = 16384L, con = mysql)),
               "TEXT")
  expect_warning(col_chr(length = 16384L, force_text = FALSE, con = mysql),
                 "possibly consider TEXT instead")
  expect_equal(as.character(col_chr(length = 65535L, con = mysql)),
               "TEXT")
  expect_equal(as.character(col_chr(length = 65536L, con = mysql)),
               "MEDIUMTEXT")
  expect_equal(as.character(col_chr(length = 16777215L, con = mysql)),
               "MEDIUMTEXT")
  expect_equal(as.character(col_chr(length = 16777216L, con = mysql)),
               "LONGTEXT")
  expect_equal(as.character(col_chr(char_set = "foo", collate = "bar",
                                    con = mysql)),
               "VARCHAR(255) CHARACTER SET 'foo' COLLATE 'bar'")
  expect_equal(as.character(col_chr(char_set = "foo", con = mysql)),
               "VARCHAR(255) CHARACTER SET 'foo'")
  expect_equal(as.character(col_chr(collate = "bar", con = mysql)),
               "VARCHAR(255) COLLATE 'bar'")
  expect_equal(as.character(col_chr(char_set = "fo'o", con = mysql)),
               "VARCHAR(255) CHARACTER SET 'fo\\'o'")
  expect_equal(as.character(col_chr(char_set = "foo\n", con = mysql)),
               "VARCHAR(255) CHARACTER SET 'foo\\n'")
  expect_error(col_chr(char_set = "", con = mysql))
})

test_that("binary data types can be specified", {
  expect_s4_class(col_raw(con = mysql),
                  "SQL")
  expect_error(col_raw(length = "foo", con = mysql))
  expect_error(col_raw(length = -1L, con = mysql))
  expect_error(col_raw(length = 256, fixed = TRUE, con = mysql))
  expect_error(col_raw(fixed = TRUE, force_blob = TRUE, con = mysql))
  expect_equal(as.character(col_raw(con = mysql)),
               "VARBINARY(255)")
  expect_equal(as.character(col_raw(fixed = TRUE, con = mysql)),
               "BINARY(255)")
  expect_equal(as.character(col_raw(force_blob = TRUE, con = mysql)),
               "TINYBLOB")
  expect_equal(as.character(col_raw(length = 16383L, con = mysql)),
               "VARBINARY(16383)")
  expect_equal(as.character(col_raw(length = 16384L, con = mysql)),
               "BLOB")
  expect_warning(col_raw(length = 16384L, force_blob = FALSE, con = mysql),
                 "possibly consider BLOB instead")
  expect_equal(as.character(col_raw(length = 65535L, con = mysql)),
               "BLOB")
  expect_equal(as.character(col_raw(length = 65536L, con = mysql)),
               "MEDIUMBLOB")
  expect_equal(as.character(col_raw(length = 16777215L, con = mysql)),
               "MEDIUMBLOB")
  expect_equal(as.character(col_raw(length = 16777216L, con = mysql)),
               "LONGBLOB")
})

test_that("boolean data types can be specified", {
  expect_s4_class(col_lgl(con = mysql),
                  "SQL")
  expect_equal(as.character(col_lgl(con = mysql)),
               "BOOL")
})

test_that("enum data types can be specified", {
  expect_s4_class(col_fct(levels = letters[1:3], con = mysql),
                  "SQL")
  expect_error(col_fct(con = mysql))
  expect_error(col_fct(levels = letters[1:3], type = "foo", con = mysql))
  expect_equal(as.character(col_fct(levels = letters[1:3], con = mysql)),
               "ENUM('a', 'b', 'c')")
  expect_equal(as.character(col_fct(levels = letters[c(1:3, 3)], con = mysql)),
               "ENUM('a', 'b', 'c')")
  expect_equal(as.character(col_fct(levels = letters[1:3], type = "set",
                                    con = mysql)),
               "SET('a', 'b', 'c')")
  expect_error(col_fct(levels = NA_character_, con = mysql))
  expect_equal(as.character(col_fct(levels = "fo'o", con = mysql)),
               "ENUM('fo\\'o')")
  expect_equal(as.character(col_fct(levels = "foo\n", con = mysql)),
               "ENUM('foo\\n')")
  expect_equal(as.character(col_fct(levels = "", con = mysql)),
               "ENUM('')")
  expect_equal(as.character(col_fct(levels = letters[1:3], char_set = "foo",
                                    collate = "bar", con = mysql)),
               "ENUM('a', 'b', 'c') CHARACTER SET 'foo' COLLATE 'bar'")
  expect_equal(as.character(col_fct(levels = letters[1:3], char_set = "foo",
                                    con = mysql)),
               "ENUM('a', 'b', 'c') CHARACTER SET 'foo'")
  expect_equal(as.character(col_fct(levels = letters[1:3], collate = "bar",
                                    con = mysql)),
               "ENUM('a', 'b', 'c') COLLATE 'bar'")
  expect_equal(as.character(col_fct(levels = letters[1:3], char_set = "fo'o",
                                    con = mysql)),
               "ENUM('a', 'b', 'c') CHARACTER SET 'fo\\'o'")
  expect_equal(as.character(col_fct(levels = letters[1:3], char_set = "foo\n",
                                    con = mysql)),
               "ENUM('a', 'b', 'c') CHARACTER SET 'foo\\n'")
  expect_equal(as.character(col_fct(levels = letters[1:3], char_set = "",
                                    con = mysql)),
               "ENUM('a', 'b', 'c')")
})

test_that("date/time data types can be specified", {
  expect_s4_class(col_dtm(con = mysql),
                  "SQL")
  expect_equal(as.character(col_dtm(con = mysql)),
               "DATETIME")
  expect_warning(col_dtm(class = "datetime", val = 2001L, con = mysql),
                 "param \"class\" will be ignored.")
  expect_error(col_dtm(val = "foo", con = mysql))
  expect_error(col_dtm(val = 1L, con = mysql))
  expect_error(col_dtm(val = 12L, con = mysql))
  expect_error(col_dtm(val = 123L, con = mysql))
  expect_equal(as.character(col_dtm(val = 1234L, con = mysql)),
               "YEAR")
  expect_error(col_dtm(val = 12345L, con = mysql))
  expect_error(col_dtm(class = "foo", con = mysql))
  expect_equal(as.character(col_dtm(val = as.POSIXlt(Sys.time()))),
               "DATETIME")
  expect_equal(col_dtm(val = as.POSIXct(Sys.time())),
               col_dtm(val = as.POSIXlt(Sys.time())))
  expect_equal(col_dtm(val = as.POSIXct(Sys.time())),
               col_dtm(val = as.POSIXlt(Sys.time())))
  expect_equal(as.character(col_dtm(val = Sys.Date())),
               "DATE")
  expect_equal(as.character(col_dtm(val = difftime(Sys.time(), Sys.Date()))),
               "TIME")
})

rm_con()