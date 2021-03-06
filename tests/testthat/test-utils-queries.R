context("utils: test query utils")

set_con(section = "mysql_unittest")
drop_db_tbl(c("plane"), force = TRUE)
write_db_tbl("plane", planes, keys = pk_spec("tailnum"), char_set = "ascii")

test_that("columns can be listed", {
  expect_error(show_db_cols(c("plane", "airport")))
  expect_error(show_db_cols("plane", full = "year"))
  expect_error(show_db_cols("plane", parse = 1L))
  expect_error(show_db_cols("plane", like = ""))
  expect_error(show_db_cols("plane", like = c("year", "type")))
  expect_error(show_db_cols("plane", where = "year"))
  expect_is(show_db_cols("plane"), "data.frame")
  expect_is(show_db_cols("plane"), "tbl_df")
  expect_equal(ncol(show_db_cols("plane")), 6L)
  expect_named(show_db_cols("plane"), c("Field", "Type", "Null", "Key",
                                        "Default", "Extra"))
  expect_equal(nrow(show_db_cols("plane")), 9L)
  expect_equal(nrow(show_db_cols("plane", like = "year")), 1L)
  expect_equal(nrow(show_db_cols("plane", where = DBI::SQL("Field = 'year'"))),
               1L)
  expect_equal(ncol(show_db_cols("plane", full = TRUE)), 9L)
  expect_named(show_db_cols("plane", full = TRUE),
               c("Field", "Type", "Collation", "Null", "Key", "Default",
                 "Extra", "Privileges", "Comment"))
  expect_equal(ncol(show_db_cols("plane", parse = TRUE)), 8L)
  expect_named(show_db_cols("plane", parse = TRUE),
               c("Field", "Type", "Length", "Unsigned", "Null", "Key",
                 "Default", "Extra"))
  expect_equal(ncol(show_db_cols("plane", full = TRUE, parse = TRUE)), 12L)
  expect_named(show_db_cols("plane", full = TRUE, parse = TRUE),
               c("Field", "Type", "Length", "Unsigned", "Charset",
                 "Collation", "Null", "Key", "Default", "Extra", "Privileges",
                 "Comment"))
  expect_is(show_db_cols("plane", parse = TRUE)$Unsigned, "logical")
  expect_is(show_db_cols("plane", parse = TRUE)$Null, "logical")
})

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

test_that("data types can be parsed", {
  expect_is(parse_data_type(col_int()), "tbl_df")
  expect_equal(parse_data_type(col_int()),
               tibble::tibble(Type = "INT", Length = NA_integer_,
                              Unsigned = FALSE))
  expect_equal(parse_data_type(c(col_int(), col_dbl())),
               tibble::tibble(Type = c("INT", "DOUBLE"), Length = NA_integer_,
                              Unsigned = FALSE))
  expect_equal(parse_data_type(c(col_int(), col_dbl(unsigned = TRUE))),
               tibble::tibble(Type = c("INT", "DOUBLE"), Length = NA_integer_,
                              Unsigned = c(FALSE, TRUE)))
  expect_equal(parse_data_type(col_chr()),
               tibble::tibble(Type = "VARCHAR", Length = 255L,
                              Unsigned = FALSE))
  expect_equal(parse_data_type(col_dtm()),
               tibble::tibble(Type = "DATETIME", Length = NA_integer_,
                              Unsigned = FALSE))
  expect_equal(parse_data_type("int"),
               tibble::tibble(Type = "INT", Length = NA_integer_,
                              Unsigned = FALSE))
  expect_equal(parse_data_type(" int"),
               tibble::tibble(Type = "INT", Length = NA_integer_,
                              Unsigned = FALSE))
  expect_equal(parse_data_type("\nint"),
               tibble::tibble(Type = "INT", Length = NA_integer_,
                              Unsigned = FALSE))
  expect_equal(parse_data_type("\nint\t"),
               tibble::tibble(Type = "INT", Length = NA_integer_,
                              Unsigned = FALSE))
  expect_equal(parse_data_type("int foo"),
               tibble::tibble(Type = "INT", Length = NA_integer_,
                              Unsigned = FALSE))
  expect_equal(parse_data_type("int(5)"),
               tibble::tibble(Type = "INT", Length = 5L, Unsigned = FALSE))
  expect_equal(parse_data_type("int(5) unsigned"),
               tibble::tibble(Type = "INT", Length = 5L, Unsigned = TRUE))
  expect_equal(parse_data_type("int(5)\tunsigned"),
               tibble::tibble(Type = "INT", Length = 5L, Unsigned = TRUE))
  expect_equal(parse_data_type("int(5)    unsigned"),
               tibble::tibble(Type = "INT", Length = 5L, Unsigned = TRUE))
  expect_equal(parse_data_type("int(5)\n\nunsigned"),
               tibble::tibble(Type = "INT", Length = 5L, Unsigned = TRUE))
  expect_equal(parse_data_type("enum('a')"),
               tibble::tibble(Type = "ENUM", Length = "a", Unsigned = FALSE))
  expect_equal(parse_data_type("enum('a', 'b')"),
               tibble::tibble(Type = "ENUM", Length = list(c("a", "b")),
                              Unsigned = FALSE))
  expect_equal(parse_data_type("enum('a, b')"),
               tibble::tibble(Type = "ENUM", Length = "a, b",
                              Unsigned = FALSE))
  expect_equal(parse_data_type("enum('(a, b)')"),
               tibble::tibble(Type = "ENUM", Length = "(a, b)",
                              Unsigned = FALSE))
  expect_equal(parse_data_type("enum('a','b')"),
               tibble::tibble(Type = "ENUM", Length = list(c("a", "b")),
                              Unsigned = FALSE))
  expect_equal(parse_data_type("enum('a',\n'b')"),
               tibble::tibble(Type = "ENUM", Length = list(c("a", "b")),
                              Unsigned = FALSE))
  expect_equal(parse_data_type("enum('a', '(b)')"),
               tibble::tibble(Type = "ENUM", Length = list(c("a", "(b)")),
                              Unsigned = FALSE))
  expect_equal(parse_data_type(c("enum('a', 'b')", "int")),
               tibble::tibble(Type = c("ENUM", "INT"),
                              Length = list(c("a", "b"), NA_character_),
               Unsigned = FALSE))
})

test_that("column definitions can be parsed", {
  expect_is(parse_col_spec(col_spec()), "tbl_df")
  expect_equal(nrow(parse_col_spec(col_spec())), 1L)
  tbl <- tibble::tibble(Field = "foo", Type = "INT", Length = NA_integer_,
                        Unsigned = FALSE, Null = TRUE,
                        Charset = NA_character_, Collation = NA_character_)
  expect_equal(parse_col_spec(col_spec("foo", "int", unsigned = FALSE,
                             nullable = TRUE)),
               tbl)
  expect_equal(parse_col_spec(col_spec("foo", "int", unsigned = FALSE,
                             nullable = TRUE, auto_increment = TRUE)),
               tbl)
  expect_equal(parse_col_spec("`foo` INT"), tbl)
  expect_error(parse_col_spec("INT"))
  tbl  <- tibble::tibble(Field = "foo", Type = "INT", Length = 5L,
                         Unsigned = TRUE, Null = TRUE,
                         Charset = NA_character_, Collation = NA_character_)
  expect_equal(parse_col_spec("`foo` INT(5) UNSIGNED"), tbl)
  expect_equal(parse_col_spec(" `foo`  INT(5) UNSIGNED"), tbl)
  expect_equal(parse_col_spec(" `foo`  INT(5)\nUNSIGNED"), tbl)
  expect_equal(parse_col_spec(c("`foo` INT(5) UNSIGNED",
                               "`foo` INT(5) UNSIGNED")), rbind(tbl, tbl))
  expect_equal(parse_col_spec(c("`foo` INT(5) UNSIGNED NULL",
                               "\nfoo double UNSIGNED",
                               "`foo` INT(5) UNSIGNED")),
               tibble::tibble(Field = "foo", Type = c("INT", "DOUBLE", "INT"),
                              Length = c(5L, NA, 5L), Unsigned = TRUE,
                              Null = TRUE, Charset = NA_character_,
                              Collation = NA_character_))
  expect_equal(parse_col_spec(col_spec("foo", "char", fixed = TRUE,
                             nullable = TRUE)),
               tibble::tibble(Field = "foo", Type = "CHAR",
                              Length = 255L, Unsigned = FALSE, Null = TRUE,
                              Charset = NA_character_,
                              Collation = NA_character_))
  expect_equal(parse_col_spec(col_spec("foo", "char", fixed = TRUE,
                             nullable = TRUE)),
               tibble::tibble(Field = "foo", Type = "CHAR",
                              Length = 255L, Unsigned = FALSE, Null = TRUE,
                              Charset = NA_character_,
                              Collation = NA_character_))
  expect_equal(parse_col_spec(col_spec("foo", "char", length = 65535L,
                             nullable = TRUE)),
               tibble::tibble(Field = "foo", Type = "TEXT",
                              Length = NA_integer_, Unsigned = FALSE,
                              Null = TRUE, Charset = NA_character_,
                              Collation = NA_character_))
  expect_equal(parse_col_spec(paste0("`foo` VARCHAR(255) CHARACTER SET 'foo' ",
                                    "COLLATE 'bar'")),
               tibble::tibble(Field = "foo", Type = "VARCHAR",
                              Length = 255L, Unsigned = FALSE,
                              Null = TRUE, Charset = "foo",
                              Collation = "bar"))
  expect_equal(parse_col_spec(c("`foo` VARCHAR(255) CHARACTER SET 'foo'",
                               "foo INT", "foo CHAR(5)",
                               "`foo` VARCHAR(255) COLLATE 'bar'")),
               tibble::tibble(Field = "foo",
                              Type = c("VARCHAR", "INT", "CHAR", "VARCHAR"),
                              Length = c(255L, NA, 5L, 255L), Unsigned = FALSE,
                              Null = TRUE, Charset = c("foo", NA, NA, NA),
                              Collation = c(NA, NA, NA, "bar")))
  expect_equal(parse_col_spec("`fo`o` INT(5) UNSIGNED"),
               tibble::tibble(Field = "fo`o", Type = "INT", Length = 5L,
                              Unsigned = TRUE, Null = TRUE,
                              Charset = NA_character_,
                              Collation = NA_character_))
  expect_equal(parse_col_spec("`fo o` INT(5) UNSIGNED"),
               tibble::tibble(Field = "fo o", Type = "INT", Length = 5L,
                              Unsigned = TRUE, Null = TRUE,
                              Charset = NA_character_,
                              Collation = NA_character_))
  expect_equal(parse_col_spec("`fo(o)` INT(5) UNSIGNED"),
               tibble::tibble(Field = "fo(o)", Type = "INT", Length = 5L,
                              Unsigned = TRUE, Null = TRUE,
                              Charset = NA_character_,
                              Collation = NA_character_))
  expect_equal(parse_col_spec(c("`fo o` INT", "foo INT")),
               tibble::tibble(Field = c("fo o", "foo"), Type = "INT",
                              Length = NA_integer_, Unsigned = FALSE,
                              Null = TRUE, Charset = NA_character_,
                              Collation = NA_character_))
})

drop_db_tbl(c("plane"), force = TRUE)
rm_con()
