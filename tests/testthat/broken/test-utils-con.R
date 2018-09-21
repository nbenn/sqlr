context("utils: test mysql db access")

test_that("config file can be loaded", {
  expect_is(sqlr:::load_config(), "list")
  expect_named(sqlr:::load_config())
  expect_gte(length(sqlr:::load_config()), 1)

  cfg_all <- sqlr:::load_config()
  cfg_1 <- sqlr:::load_config()["db_setup"]
  cfg_2 <- sqlr:::load_config()["mysql_unittest"]

  writeLines(yaml::as.yaml(cfg_1), con = "foo.yaml")
  expect_equal(sqlr:::load_config(), cfg_1)
  unlink("foo.yaml")

  writeLines(yaml::as.yaml(cfg_1), con = "foo.yml")
  expect_equal(sqlr:::load_config(), cfg_1)
  unlink("foo.yml")

  writeLines(yaml::as.yaml(cfg_1), con = "foo.bar")
  expect_equal(sqlr:::load_config(), cfg_all)
  unlink("foo.bar")

  writeLines(yaml::as.yaml(cfg_1), con = "foo.yml")
  writeLines(yaml::as.yaml(cfg_2), con = "bar.yml")
  expect_equal(sqlr:::load_config(), cfg_2)
  expect_equal(sqlr:::load_config("foo.yml"), cfg_1)
  unlink(c("foo.yml", "bar.yml"))

  sec <- list(
    a = 1,
    b = list(c = 2, d = 3),
    e = list(
      f = list(g = 4, h = 5),
      i = 6
    ),
    j = 7,
    k = list(
      l = 8,
      m = list(
        n = 9,
        o = list(o = 10, p = 11),
        q = 12
      ),
      r = 13
    ),
    m = list(1, 2),
    s = 14,
    c = 15,
    t = list(e = 16)
  )
  writeLines(yaml::as.yaml(sec), con = "foo.yaml")
  expect_equal(sqlr:::load_config(section = "f"), list(g = 4, h = 5))
  expect_equal(sqlr:::load_config(section = "m"), 1:2)
  expect_equal(sqlr:::load_config(section = "c"), 15)
  expect_equal(sqlr:::load_config(section = "o"), list(o = 10, p = 11))
  expect_equal(sqlr:::load_config(section = "q"), 12)
  expect_equal(sqlr:::load_config(section = "d"), 3)
  expect_equal(sqlr:::load_config(section = "e"), list(
    f = list(g = 4, h = 5),
    i = 6
  ))
  expect_error(sqlr:::load_config(section = "z"))
  unlink("foo.yaml")
})

test_that("db connection can be established", {
  cfg <- sqlr:::load_config(section = "mysql_unittest")
  expect_named(cfg,
    c("dbtype", "dbname", "username", "password"),
    ignore.order = TRUE
  )
  expect_equal(cfg$dbtype, "mysql")
  expect_equal(cfg$dbname, "testthat")
  expect_equal(cfg$username, "test")
  expect_equal(cfg$password, "test123")

  con <- sqlr:::connect_mysql(
    dbname = "testthat", username = "test",
    password = "test123"
  )
  expect_s4_class(con, "MariaDBConnection")
  DBI::dbDisconnect(con)

  con <- sqlr:::connect_db(cfg)
  expect_s4_class(con, "MariaDBConnection")
  DBI::dbDisconnect(con)
})

test_that("get/set/rm config con", {
  expect_null(rm_con())
  expect_null(sqlr:::config$con)
  expect_true(set_con(section = "mysql_unittest"))
  expect_s4_class(sqlr:::config$con, "MariaDBConnection")
  expect_equal(sqlr:::get_con(), sqlr:::config$con)
  expect_false(set_con(section = "mysql_unittest"))
  expect_true(set_con(section = "mysql_unittest", update = TRUE))
  expect_null(rm_con())
})
