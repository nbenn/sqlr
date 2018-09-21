context("mysql table specification")

setup(
  set_con(section = "mysql_unittest")
)

test_that("tables can be specified", {
  expect_is(
    tbl_spec(),
    "sqlr_tbl_spec"
  )
  expect_error(tbl_spec(""))
  expect_error_render(tbl_spec("foo", "bar"))
  expect_error(tbl_spec(c("foo", "bar")))
  expect_error_render(tbl_spec("foo", keys = "bar"))
  expect_error(tbl_spec("foo", temp = "bar"))
  expect_error_render(tbl_spec("foo", table_options = "bar"))
  expect_render(
    tbl_spec("foo"),
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
  expect_render(
    tbl_spec("foo", temp = TRUE),
    paste(
      "CREATE TEMPORARY TABLE `foo` (`id` INT UNSIGNED NOT",
      "NULL AUTO_INCREMENT PRIMARY KEY) ENGINE = 'InnoDB'"
    )
  )
  expect_render(
    tbl_spec("foo", force = TRUE),
    paste(
      "CREATE TABLE IF NOT EXISTS `foo` (`id` INT UNSIGNED NOT",
      "NULL AUTO_INCREMENT PRIMARY KEY) ENGINE = 'InnoDB'"
    )
  )
  expect_render(
    tbl_spec(
      "foo",
      list(
        col_spec("bar"),
        col_spec("baz", col_int())
      )
    ),
    paste(
      "CREATE TABLE `foo` (`bar` INT, `baz` INT)",
      "ENGINE = 'InnoDB'"
    )
  )
  expect_render(
    tbl_spec("foo", auto_incr = 5L),
    paste(
      "CREATE TABLE `foo` (`id` INT UNSIGNED NOT NULL",
      "AUTO_INCREMENT PRIMARY KEY) ENGINE = 'InnoDB'",
      "AUTO_INCREMENT = 5"
    )
  )
  expect_render(
    tbl_spec("foo", char_set = "foobar"),
    paste(
      "CREATE TABLE `foo` (`id` INT UNSIGNED NOT NULL",
      "AUTO_INCREMENT PRIMARY KEY) ENGINE = 'InnoDB'",
      "DEFAULT CHARACTER SET = foobar"
    )
  )
  expect_render(
    tbl_spec("foo", col_spec("id"), pk_spec("id")),
    paste(
      "CREATE TABLE `foo` (`id` INT, PRIMARY KEY (`id`))",
      "ENGINE = 'InnoDB'"
    )
  )
  expect_render(
    tbl_spec(
      "foo", col_spec("id"),
      list(key_spec("id"), uk_spec("id"))
    ),
    paste(
      "CREATE TABLE `foo` (`id` INT, KEY (`id`), UNIQUE KEY",
      "(`id`)) ENGINE = 'InnoDB'"
    )
  )
})

teardown(
  rm_con()
)