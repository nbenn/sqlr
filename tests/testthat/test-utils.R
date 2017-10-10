context("utils: test mysql db access")

test_that("config file can be loaded", {
  expect_is(sqlr:::load_config(), "list")
  expect_named(sqlr:::load_config())
  expect_gte(length(sqlr:::load_config()), 1)

  cfg <- sqlr:::get_cfg(section = "mysql_unittest")

  expect_named(cfg,
               c("dbtype", "dbname", "username", "password"),
               ignore.order = TRUE)
  expect_equal(cfg$dbtype, "mysql")
  expect_equal(cfg$dbname, "testthat")
  expect_equal(cfg$username, "test")
  expect_equal(cfg$password, "test123")
  expect_equal(cfg, sqlr:::config$cfg[["mysql_unittest"]])
})

test_that("db connection can be established", {
  cfg <- sqlr:::get_cfg(section = "mysql_unittest")
  expect_s4_class(sqlr:::connect_db(cfg), "MariaDBConnection")
})