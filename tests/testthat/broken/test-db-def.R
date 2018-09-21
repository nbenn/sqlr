context("data definition functions")

set_con(section = "mysql_unittest")

test_that("mysql tables can be created", {
  expect_true(create_db_tbl("iris", get_col_spec(iris)))
  expect_true("iris" %in% DBI::dbListTables(sqlr:::get_con()))
  expect_true(all(colnames(iris) == show_db_cols("iris")$Field))
  expect_true(drop_db_tbl("iris"))
})

rm_con()