
test_that("integer sql generation", {

  res <- render_sql(smallint(), simulate_postgres())

  expect_s4_class(res, "SQL")

  expect_identical(as.character(res), "smallint")
})
