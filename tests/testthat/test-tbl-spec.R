context("mysql table specification")

mysql <- sqlr:::get_con(section = "mysql_unittest")

test_that("tables can be specified", {
  expect_s4_class(tbl_spec(con = mysql),
                  "SQL")
  expect_error(tbl_spec("", con = mysql))
  expect_error(tbl_spec("foo", "bar", con = mysql))
  expect_error(tbl_spec(c("foo", "bar"), con = mysql))
  expect_error(tbl_spec("foo", keys = "bar", con = mysql))
  expect_error(tbl_spec("foo", temp = "bar", con = mysql))
  expect_error(tbl_spec("foo", table_options = "bar", con = mysql))
  expect_equal(as.character(tbl_spec("foo", con = mysql)),
               paste("CREATE TABLE `foo` (`id` INT UNSIGNED NOT NULL",
                     "AUTO_INCREMENT PRIMARY KEY) ENGINE = 'InnoDB'"))
  expect_equal(tbl_spec("foo", con = mysql),
               tbl_spec("foo",
                        col_spec("id", type = "int", nullable = FALSE,
                                 auto_increment = TRUE, unsigned = TRUE,
                                 key = "primary"),
                        con = mysql))
  expect_equal(as.character(tbl_spec("foo", temp = TRUE, con = mysql)),
               paste("CREATE TEMPORARY TABLE `foo` (`id` INT UNSIGNED NOT",
                     "NULL AUTO_INCREMENT PRIMARY KEY) ENGINE = 'InnoDB'"))
  expect_equal(as.character(tbl_spec("foo", force = TRUE, con = mysql)),
               paste("CREATE TABLE IF NOT EXISTS `foo` (`id` INT UNSIGNED NOT",
                     "NULL AUTO_INCREMENT PRIMARY KEY) ENGINE = 'InnoDB'"))
  expect_equal(as.character(tbl_spec("foo",
                                     spec_lst(col_spec("bar"),
                                              col_spec("baz", col_int())),
                                     con = mysql)),
               paste("CREATE TABLE `foo` (`bar` INT, `baz` INT)",
                     "ENGINE = 'InnoDB'"))
  expect_equal(as.character(tbl_spec("foo", auto_incr = 5L, con = mysql)),
               paste("CREATE TABLE `foo` (`id` INT UNSIGNED NOT NULL",
                     "AUTO_INCREMENT PRIMARY KEY) ENGINE = 'InnoDB'",
                     "AUTO_INCREMENT = 5"))
  expect_equal(as.character(tbl_spec("foo", char_set = "foobar", con = mysql)),
               paste("CREATE TABLE `foo` (`id` INT UNSIGNED NOT NULL",
                     "AUTO_INCREMENT PRIMARY KEY) ENGINE = 'InnoDB'",
                     "DEFAULT CHARACTER SET = foobar"))
  expect_equal(as.character(tbl_spec("foo", col_spec("id"), pk_spec("id"),
                                     con = mysql)),
               paste("CREATE TABLE `foo` (`id` INT, PRIMARY KEY (`id`))",
                     "ENGINE = 'InnoDB'"))
  expect_equal(as.character(tbl_spec("foo", col_spec("id"),
                                     spec_lst(key_spec("id"), uk_spec("id")),
                                     con = mysql)),
               paste("CREATE TABLE `foo` (`id` INT, KEY (`id`), UNIQUE KEY",
                     "(`id`)) ENGINE = 'InnoDB'"))
})

rm_con()
