test_that("an unregistered connection class is an error, not a fallback", {
  fake <- structure(list(), class = "NoSuchConnection")

  expect_error(sqlr_for(fake), "no sqlr dialect registered")
})

test_that("a registered dialect resolves", {
  sqlr_register_dialect("FakeConnection", function(con) test_dialect())
  fake <- structure(list(), class = "FakeConnection")

  expect_s3_class(sqlr_for(fake), "sqlr::sqlr_dialect")
})
