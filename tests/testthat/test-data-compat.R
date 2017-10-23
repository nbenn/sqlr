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

test_that("mysql strings can be unqouted", {
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), "foo")),
               "foo")
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(),
                                                 c("foo", "bar"))),
               c("foo", "bar"))
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), "fo'o")),
               "fo'o")
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), "fo''o")),
               "fo''o")
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), "'fo''o")),
               "'fo''o")
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), "fo`o")),
               "fo`o")
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), "`foo")),
               "`foo")
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), "fo``o")),
               "fo``o")
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), "fo```o")),
               "fo```o")
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), "fo\no")),
               "fo\no")
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), "fo\ro")),
               "fo\ro")
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), "fo\to")),
               "fo\to")
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), "fo\\o")),
               "fo\\o")
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), "fo\\\\o")),
               "fo\\\\o")
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), 'fo"o')),
               'fo"o')
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), '"fo"o')),
               '"fo"o')
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), "fo\"o")),
               'fo"o')
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), "fo\"o")),
               "fo\"o")
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), "fo\x1ao")),
               "fo\x1ao")
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), "fo\032o")),
               "fo\032o")
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), "fo-$!o")),
               "fo-$!o")
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), "öääü")),
               "öääü")
  expect_equal(unquote_string(DBI::dbQuoteString(sqlr:::get_con(), "@ª]|{")),
               "@ª]|{")
})

test_that("column definitions can be parsed", {
  expect_is(parse_col_def(col_spec()), "tbl_df")
  expect_equal(nrow(parse_col_def(col_spec())), 1L)
  tbl <- tibble::tibble(Field = "foo", Type = "INT", Length = NA_integer_,
                        Unsigned = FALSE, Null = TRUE)
  expect_equal(parse_col_def(col_spec("foo", "int", unsigned = FALSE,
                             nullable = TRUE)),
               tbl)
  expect_equal(parse_col_def(col_spec("foo", "int", unsigned = FALSE,
                             nullable = TRUE, auto_increment = TRUE)),
               tbl)
  expect_equal(parse_col_def("`foo` INT"), tbl)
  expect_equal(parse_col_def("INT"),
               tibble::tibble(Field = NA_character_, Type = "INT",
                              Length = NA_integer_, Unsigned = FALSE,
                              Null = TRUE))
  tbl  <- tibble::tibble(Field = NA_character_, Type = "INT", Length = 5L,
                         Unsigned = TRUE, Null = TRUE)
  expect_equal(parse_col_def("INT(5) UNSIGNED"), tbl)
  expect_equal(parse_col_def("INT(5)  UNSIGNED"), tbl)
  expect_equal(parse_col_def("INT(5)\nUNSIGNED"), tbl)
  expect_equal(parse_col_def("INT(5)\tUNSIGNED"), tbl)
  expect_equal(parse_col_def(" INT(5) UNSIGNED"), tbl)
  tbl  <- tibble::tibble(Field = "foo", Type = "INT", Length = 5L,
                         Unsigned = TRUE, Null = TRUE)
  expect_equal(parse_col_def("`foo` INT(5) UNSIGNED"), tbl)
  expect_equal(parse_col_def(" `foo`  INT(5) UNSIGNED"), tbl)
  expect_equal(parse_col_def(" `foo`  INT(5)\nUNSIGNED"), tbl)
  expect_equal(parse_col_def(c("`foo` INT(5) UNSIGNED",
                               "`foo` INT(5) UNSIGNED")), rbind(tbl, tbl))
  expect_equal(parse_col_def(c("`foo` INT(5) UNSIGNED NULL",
                               "\tINT(5) UNSIGNED",
                               "`foo` INT(5) UNSIGNED")),
               tibble::tibble(Field = c("foo", NA, "foo"), Type = "INT",
                              Length = 5L, Unsigned = TRUE, Null = TRUE))
  expect_equal(parse_col_def(col_spec("foo", "char", fixed = TRUE,
                             nullable = TRUE)),
               tibble::tibble(Field = "foo", Type = "CHAR",
                              Length = 255L, Unsigned = FALSE, Null = TRUE))
  expect_equal(parse_col_def(col_spec("foo", "char", fixed = TRUE,
                             nullable = TRUE)),
               tibble::tibble(Field = "foo", Type = "CHAR",
                              Length = 255L, Unsigned = FALSE, Null = TRUE))
  expect_equal(parse_col_def(col_spec("foo", "char", length = 65535L,
                             nullable = TRUE)),
               tibble::tibble(Field = "foo", Type = "TEXT",
                              Length = NA_integer_, Unsigned = FALSE,
                              Null = TRUE))
  expect_equal(parse_col_def("`foo` ENUM('a', 'b') CHARACTER SET ascii"),
               tibble::tibble(Field = "foo", Type = "ENUM",
                              Length = list(c("'a'", "'b'")), Unsigned = FALSE,
                              Null = TRUE))
})

rm_con()
