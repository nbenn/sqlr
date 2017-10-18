context("data definition functions")

mysql <- sqlr:::get_con(section = "mysql_unittest")

test_that("mysql tables can be created", {
  expect_true(create_db_tbl("iris", get_col_spec(iris), con = mysql))
  expect_true("iris" %in% DBI::dbListTables(mysql))
  expect_true(all(colnames(iris) == show_db_cols("iris", con = mysql)$Field))
  expect_true(drop_db_tbl("iris"))
})