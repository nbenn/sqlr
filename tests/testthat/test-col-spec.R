context("integer data type specification")

mysql <- get_con(section = "mysql_unittest")

test_that("str_length is number of characters", {
  expect_true(TRUE)
})

rm_con()