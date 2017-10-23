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
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), "foo")),
               "foo")
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(),
                                                 c("foo", "bar"))),
               c("foo", "bar"))
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), "fo'o")),
               "fo'o")
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), "fo''o")),
               "fo''o")
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), "'fo''o")),
               "'fo''o")
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), "fo`o")),
               "fo`o")
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), "`foo")),
               "`foo")
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), "fo``o")),
               "fo``o")
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), "fo```o")),
               "fo```o")
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), "fo\no")),
               "fo\no")
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), "fo\ro")),
               "fo\ro")
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), "fo\to")),
               "fo\to")
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), "fo\\o")),
               "fo\\o")
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), "fo\\\\o")),
               "fo\\\\o")
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), 'fo"o')),
               'fo"o')
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), '"fo"o')),
               '"fo"o')
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), "fo\"o")),
               'fo"o')
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), "fo\"o")),
               "fo\"o")
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), "fo\x1ao")),
               "fo\x1ao")
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), "fo\032o")),
               "fo\032o")
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), "fo-$!o")),
               "fo-$!o")
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), "öääü")),
               "öääü")
  expect_equal(unquote_str(DBI::dbQuoteString(sqlr:::get_con(), "@ª]|{")),
               "@ª]|{")
})

test_that("column definitions can be parsed", {
  expect_is(parse_col_spec(col_spec()), "tbl_df")
  expect_equal(nrow(parse_col_spec(col_spec())), 1L)
  tbl <- tibble::tibble(Field = "foo", Type = "INT", Length = NA_integer_,
                        Unsigned = FALSE, Null = TRUE,
                        Encoding = NA_character_, Collation = NA_character_)
  expect_equal(parse_col_spec(col_spec("foo", "int", unsigned = FALSE,
                             nullable = TRUE)),
               tbl)
  expect_equal(parse_col_spec(col_spec("foo", "int", unsigned = FALSE,
                             nullable = TRUE, auto_increment = TRUE)),
               tbl)
  expect_equal(parse_col_spec("`foo` INT"), tbl)
  expect_equal(parse_col_spec("INT"),
               tibble::tibble(Field = NA_character_, Type = "INT",
                              Length = NA_integer_, Unsigned = FALSE,
                              Null = TRUE, Encoding = NA_character_,
                              Collation = NA_character_))
  tbl  <- tibble::tibble(Field = NA_character_, Type = "INT", Length = 5L,
                         Unsigned = TRUE, Null = TRUE,
                         Encoding = NA_character_, Collation = NA_character_)
  expect_equal(parse_col_spec("INT(5) UNSIGNED"), tbl)
  expect_equal(parse_col_spec("INT(5)  UNSIGNED"), tbl)
  expect_equal(parse_col_spec("INT(5)\nUNSIGNED"), tbl)
  expect_equal(parse_col_spec("INT(5)\tUNSIGNED"), tbl)
  expect_equal(parse_col_spec(" INT(5) UNSIGNED"), tbl)
  tbl  <- tibble::tibble(Field = "foo", Type = "INT", Length = 5L,
                         Unsigned = TRUE, Null = TRUE,
                         Encoding = NA_character_, Collation = NA_character_)
  expect_equal(parse_col_spec("`foo` INT(5) UNSIGNED"), tbl)
  expect_equal(parse_col_spec(" `foo`  INT(5) UNSIGNED"), tbl)
  expect_equal(parse_col_spec(" `foo`  INT(5)\nUNSIGNED"), tbl)
  expect_equal(parse_col_spec(c("`foo` INT(5) UNSIGNED",
                               "`foo` INT(5) UNSIGNED")), rbind(tbl, tbl))
  expect_equal(parse_col_spec(c("`foo` INT(5) UNSIGNED NULL",
                               "\tINT(5) UNSIGNED",
                               "`foo` INT(5) UNSIGNED")),
               tibble::tibble(Field = c("foo", NA, "foo"), Type = "INT",
                              Length = 5L, Unsigned = TRUE, Null = TRUE,
                              Encoding = NA_character_,
                              Collation = NA_character_))
  expect_equal(parse_col_spec(col_spec("foo", "char", fixed = TRUE,
                             nullable = TRUE)),
               tibble::tibble(Field = "foo", Type = "CHAR",
                              Length = 255L, Unsigned = FALSE, Null = TRUE,
                              Encoding = NA_character_,
                              Collation = NA_character_))
  expect_equal(parse_col_spec(col_spec("foo", "char", fixed = TRUE,
                             nullable = TRUE)),
               tibble::tibble(Field = "foo", Type = "CHAR",
                              Length = 255L, Unsigned = FALSE, Null = TRUE,
                              Encoding = NA_character_,
                              Collation = NA_character_))
  expect_equal(parse_col_spec(col_spec("foo", "char", length = 65535L,
                             nullable = TRUE)),
               tibble::tibble(Field = "foo", Type = "TEXT",
                              Length = NA_integer_, Unsigned = FALSE,
                              Null = TRUE, Encoding = NA_character_,
                              Collation = NA_character_))
  expect_equal(parse_col_spec("`foo` ENUM('a')"),
               tibble::tibble(Field = "foo", Type = "ENUM",
                              Length = "a", Unsigned = FALSE,
                              Null = TRUE, Encoding = NA_character_,
                              Collation = NA_character_))
  expect_equal(parse_col_spec("`foo` ENUM('a, b')"),
               tibble::tibble(Field = "foo", Type = "ENUM",
                              Length = "a, b", Unsigned = FALSE,
                              Null = TRUE, Encoding = NA_character_,
                              Collation = NA_character_))
  expect_equal(parse_col_spec("`foo` ENUM('a', 'b')"),
               tibble::tibble(Field = "foo", Type = "ENUM",
                              Length = list(c("a", "b")), Unsigned = FALSE,
                              Null = TRUE, Encoding = NA_character_,
                              Collation = NA_character_))
  expect_equal(parse_col_spec(c("`foo` ENUM('a', 'b')", "int")),
               tibble::tibble(Field = c("foo", NA), Type = c("ENUM", "INT"),
                              Length = list(c("a", "b"), NA_character_),
                              Unsigned = FALSE, Null = TRUE,
                              Encoding = NA_character_,
                              Collation = NA_character_))
  expect_equal(parse_col_spec("`foo` ENUM('a (b)')"),
               tibble::tibble(Field = "foo", Type = "ENUM",
                              Length = "a (b)", Unsigned = FALSE,
                              Null = TRUE, Encoding = NA_character_,
                              Collation = NA_character_))
  expect_equal(parse_col_spec(paste0("`foo` VARCHAR(255) CHARACTER SET 'foo' ",
                                    "COLLATE 'bar'")),
               tibble::tibble(Field = "foo", Type = "VARCHAR",
                              Length = 255, Unsigned = FALSE,
                              Null = TRUE, Encoding = "foo",
                              Collation = "bar"))
  expect_equal(parse_col_spec(c("`foo` VARCHAR(255) CHARACTER SET 'foo'",
                               "INT", "CHAR(5)",
                               "`foo` VARCHAR(255) COLLATE 'bar'")),
               tibble::tibble(Field = c("foo", NA, NA, "foo"),
                              Type = c("VARCHAR", "INT", "CHAR", "VARCHAR"),
                              Length = c(255, NA, 5, 255), Unsigned = FALSE,
                              Null = TRUE, Encoding = c("foo", NA, NA, NA),
                              Collation = c(NA, NA, NA, "bar")))
})

rm_con()
