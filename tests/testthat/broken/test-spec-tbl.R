context("mysql table specification")

set_con(section = "mysql_unittest")

test_that("tables can be specified", {
  expect_s4_class(
    tbl_spec(),
    "SQL"
  )
  expect_error(tbl_spec(""))
  expect_error(tbl_spec("foo", "bar"))
  expect_error(tbl_spec(c("foo", "bar")))
  expect_error(tbl_spec("foo", keys = "bar"))
  expect_error(tbl_spec("foo", temp = "bar"))
  expect_error(tbl_spec("foo", table_options = "bar"))
  expect_equal(
    as.character(tbl_spec("foo")),
    paste(
      "CREATE TABLE `foo` (`id` INT UNSIGNED NOT NULL",
      "AUTO_INCREMENT PRIMARY KEY) ENGINE = 'InnoDB'"
    )
  )
  expect_equal(
    tbl_spec("foo"),
    tbl_spec(
      "foo",
      col_spec("id",
        type = "int", nullable = FALSE,
        auto_increment = TRUE, unsigned = TRUE,
        key = "primary"
      )
    )
  )
  expect_equal(
    as.character(tbl_spec("foo", temp = TRUE)),
    paste(
      "CREATE TEMPORARY TABLE `foo` (`id` INT UNSIGNED NOT",
      "NULL AUTO_INCREMENT PRIMARY KEY) ENGINE = 'InnoDB'"
    )
  )
  expect_equal(
    as.character(tbl_spec("foo", force = TRUE)),
    paste(
      "CREATE TABLE IF NOT EXISTS `foo` (`id` INT UNSIGNED NOT",
      "NULL AUTO_INCREMENT PRIMARY KEY) ENGINE = 'InnoDB'"
    )
  )
  expect_equal(
    as.character(tbl_spec(
      "foo",
      list(
        col_spec("bar"),
        col_spec("baz", col_int())
      )
    )),
    paste(
      "CREATE TABLE `foo` (`bar` INT, `baz` INT)",
      "ENGINE = 'InnoDB'"
    )
  )
  expect_equal(
    as.character(tbl_spec("foo", auto_incr = 5L)),
    paste(
      "CREATE TABLE `foo` (`id` INT UNSIGNED NOT NULL",
      "AUTO_INCREMENT PRIMARY KEY) ENGINE = 'InnoDB'",
      "AUTO_INCREMENT = 5"
    )
  )
  expect_equal(
    as.character(tbl_spec("foo", char_set = "foobar")),
    paste(
      "CREATE TABLE `foo` (`id` INT UNSIGNED NOT NULL",
      "AUTO_INCREMENT PRIMARY KEY) ENGINE = 'InnoDB'",
      "DEFAULT CHARACTER SET = foobar"
    )
  )
  expect_equal(
    as.character(tbl_spec("foo", col_spec("id"), pk_spec("id"))),
    paste(
      "CREATE TABLE `foo` (`id` INT, PRIMARY KEY (`id`))",
      "ENGINE = 'InnoDB'"
    )
  )
  expect_equal(
    as.character(tbl_spec(
      "foo", col_spec("id"),
      list(key_spec("id"), uk_spec("id"))
    )),
    paste(
      "CREATE TABLE `foo` (`id` INT, KEY (`id`), UNIQUE KEY",
      "(`id`)) ENGINE = 'InnoDB'"
    )
  )
})

rm_con()
