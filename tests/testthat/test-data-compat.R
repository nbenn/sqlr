context("mysql data compatibility")

set_con(section = "mysql_unittest")

test_that("mysql identifiers can be unqouted", {
  expect_equal(unquote_ident(DBI::dbQuoteIdentifier(sqlr:::get_con(), "foo")),
               "foo")
  expect_equal(unquote_ident(DBI::dbQuoteIdentifier(sqlr:::get_con(),
                                                    c("foo", "bar"))),
               c("foo", "bar"))
  expect_equal(unquote_ident(DBI::dbQuoteIdentifier(sqlr:::get_con(), "fo'o")),
               "fo'o")
  expect_equal(unquote_ident(DBI::dbQuoteIdentifier(sqlr:::get_con(), "fo`o")),
               "fo`o")
  expect_equal(unquote_ident(DBI::dbQuoteIdentifier(sqlr:::get_con(), "`foo")),
               "`foo")
  expect_equal(unquote_ident(DBI::dbQuoteIdentifier(sqlr:::get_con(),
                                                    "fo``o")),
               "fo``o")
  expect_equal(unquote_ident(DBI::dbQuoteIdentifier(sqlr:::get_con(),
                                                    "fo```o")),
               "fo```o")
  expect_equal(unquote_ident(DBI::dbQuoteIdentifier(sqlr:::get_con(),
                                                    "fo\no")),
               "fo\no")
  expect_equal(unquote_ident(DBI::dbQuoteIdentifier(sqlr:::get_con(),
                                                    "fo-$!o")),
               "fo-$!o")
})

rm_con()
