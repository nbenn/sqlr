context("data definition functions")

setup({
  set_con(section = "mysql_unittest")
  drop_db_tbl("iris", force = TRUE, con = get_con())
})

test_that("mysql tables can be created", {
  expect_true(create_db_tbl("iris", get_col_spec(iris), con = get_con()))
  expect_true("iris" %in% DBI::dbListTables(get_con()))
  expect_true(all(colnames(iris) == show_db_cols("iris", con = get_con())$Field))
  expect_true(drop_db_tbl("iris", con = get_con()))
})

teardown({
  drop_db_tbl("iris", force = TRUE, con = get_con())
  rm_con()
})
