context("mysql reference specification")

set_con(section = "mysql_unittest")

test_that("references can be specified", {
  expect_error(fk_spec())
  expect_error(fk_spec("foo", "bar", "foobar"))
  expect_error(fk_spec("foo", c("bar", "baz"), "foobar"))
  expect_s4_class(
    fk_spec("foo", "bar", "foobar", check = FALSE),
    "SQL"
  )
  expect_warning(fk_spec("foo", "bar", "foobar",
    match = "simple",
    check = FALSE
  ))
  expect_error(fk_spec("foo", "bar", "foobar",
    match = "xyzzy",
    check = FALSE
  ))
  expect_equal(
    as.character(fk_spec("foo", "bar", "foobar", check = FALSE)),
    paste(
      "FOREIGN KEY (`foo`) REFERENCES `bar` (`foobar`) ON",
      "DELETE CASCADE ON UPDATE CASCADE"
    )
  )
  expect_equal(
    as.character(fk_spec(c("f", "o", "o"), "foobar",
      c("b", "a", "r"),
      check = FALSE
    )),
    paste(
      "FOREIGN KEY (`f`, `o`, `o`) REFERENCES `foobar`",
      "(`b`, `a`, `r`) ON DELETE CASCADE ON UPDATE CASCADE"
    )
  )
  expect_equal(
    as.character(fk_spec("foo", "bar", "foobar", "xyzzy",
      check = FALSE
    )),
    paste(
      "CONSTRAINT `xyzzy` FOREIGN KEY (`foo`) REFERENCES `bar`",
      "(`foobar`) ON DELETE CASCADE ON UPDATE CASCADE"
    )
  )
  expect_equal(
    as.character(fk_spec("foo", "bar", "foobar", "xyzzy",
      check = FALSE
    )),
    paste(
      "CONSTRAINT `xyzzy` FOREIGN KEY (`foo`) REFERENCES `bar`",
      "(`foobar`) ON DELETE CASCADE ON UPDATE CASCADE"
    )
  )
  expect_equal(
    as.character(fk_spec("foo", "bar", "baz", "foobar", "xyzzy",
      check = FALSE
    )),
    paste(
      "CONSTRAINT `foobar` FOREIGN KEY `xyzzy` (`foo`)",
      "REFERENCES `bar` (`baz`) ON DELETE CASCADE ON UPDATE",
      "CASCADE"
    )
  )
  expect_equal(
    as.character(fk_spec("fo'o", "b\nar", "foo`bar",
      check = FALSE
    )),
    paste(
      "FOREIGN KEY (`fo'o`) REFERENCES `b\nar` (`foo``bar`) ON",
      "DELETE CASCADE ON UPDATE CASCADE"
    )
  )
  expect_error(fk_spec("foo", "bar", "foobar",
    on_upd = "xyzzy",
    check = FALSE
  ))
  expect_equal(
    as.character(fk_spec("foo", "bar", "foobar",
      on_del = "set_null", on_upd = "no_action",
      check = FALSE
    )),
    paste(
      "FOREIGN KEY (`foo`) REFERENCES `bar` (`foobar`) ON",
      "DELETE SET NULL ON UPDATE NO ACTION"
    )
  )
})

test_that("primary keys can be specified", {
  expect_error(pk_spec())
  expect_error(pk_spec("foo", type = "bar"))
  expect_error(pk_spec("foo", constr_name = 1L))
  expect_error(pk_spec("foo", block_size = 1:2))
  expect_s4_class(
    pk_spec("foo"),
    "SQL"
  )
  expect_equal(
    as.character(pk_spec("foo")),
    "PRIMARY KEY (`foo`)"
  )
  expect_equal(
    as.character(pk_spec(c("foo", "bar"))),
    "PRIMARY KEY (`foo`, `bar`)"
  )
  expect_equal(
    as.character(pk_spec("fo'o")),
    "PRIMARY KEY (`fo'o`)"
  )
  expect_equal(
    as.character(pk_spec("b\nar")),
    "PRIMARY KEY (`b\nar`)"
  )
  expect_equal(
    as.character(pk_spec("foo`bar")),
    "PRIMARY KEY (`foo``bar`)"
  )
  expect_equal(
    as.character(pk_spec("foo", type = "btree")),
    "PRIMARY KEY USING BTREE (`foo`)"
  )
  expect_equal(
    as.character(pk_spec("foo", constr_name = "bar")),
    "CONSTRAINT `bar` PRIMARY KEY (`foo`)"
  )
  expect_equal(
    as.character(pk_spec("foo", constr_name = "b`ar")),
    "CONSTRAINT `b``ar` PRIMARY KEY (`foo`)"
  )
  expect_equal(
    as.character(pk_spec("foo", block_size = 4L)),
    "PRIMARY KEY (`foo`) KEY_BLOCK_SIZE = 4"
  )
  expect_equal(
    as.character(pk_spec("foo", comment = "foobar")),
    "PRIMARY KEY (`foo`) COMMENT 'foobar'"
  )
  expect_equal(
    as.character(pk_spec("foo", comment = "fo`obar")),
    "PRIMARY KEY (`foo`) COMMENT 'fo`obar'"
  )
  expect_equal(
    as.character(pk_spec("foo", comment = "fo\nobar")),
    "PRIMARY KEY (`foo`) COMMENT 'fo\\nobar'"
  )
  expect_equal(
    as.character(pk_spec("foo", comment = "fo'obar")),
    "PRIMARY KEY (`foo`) COMMENT 'fo\\'obar'"
  )
})

test_that("unique keys can be specified", {
  expect_error(uk_spec())
  expect_error(uk_spec("foo", type = "bar"))
  expect_error(uk_spec("foo", constr_name = 1L))
  expect_error(uk_spec("foo", block_size = 1:2))
  expect_s4_class(
    uk_spec("foo"),
    "SQL"
  )
  expect_equal(
    as.character(uk_spec("foo")),
    "UNIQUE KEY (`foo`)"
  )
  expect_equal(
    as.character(uk_spec(c("foo", "bar"))),
    "UNIQUE KEY (`foo`, `bar`)"
  )
  expect_equal(
    as.character(uk_spec("fo'o")),
    "UNIQUE KEY (`fo'o`)"
  )
  expect_equal(
    as.character(uk_spec("b\nar")),
    "UNIQUE KEY (`b\nar`)"
  )
  expect_equal(
    as.character(uk_spec("foo`bar")),
    "UNIQUE KEY (`foo``bar`)"
  )
  expect_equal(
    as.character(uk_spec("foo", type = "btree")),
    "UNIQUE KEY USING BTREE (`foo`)"
  )
  expect_equal(
    as.character(uk_spec("foo", constr_name = "bar")),
    "CONSTRAINT `bar` UNIQUE KEY (`foo`)"
  )
  expect_equal(
    as.character(uk_spec("foo", constr_name = "b`ar")),
    "CONSTRAINT `b``ar` UNIQUE KEY (`foo`)"
  )
  expect_equal(
    as.character(uk_spec("foo", block_size = 4L)),
    "UNIQUE KEY (`foo`) KEY_BLOCK_SIZE = 4"
  )
  expect_equal(
    as.character(uk_spec("foo", comment = "foobar")),
    "UNIQUE KEY (`foo`) COMMENT 'foobar'"
  )
  expect_equal(
    as.character(uk_spec("foo", comment = "fo`obar")),
    "UNIQUE KEY (`foo`) COMMENT 'fo`obar'"
  )
  expect_equal(
    as.character(uk_spec("foo", comment = "fo\nobar")),
    "UNIQUE KEY (`foo`) COMMENT 'fo\\nobar'"
  )
  expect_equal(
    as.character(uk_spec("foo", comment = "fo'obar")),
    "UNIQUE KEY (`foo`) COMMENT 'fo\\'obar'"
  )
  expect_equal(
    as.character(uk_spec("foo", "bar", "baz")),
    "CONSTRAINT `bar` UNIQUE KEY `baz` (`foo`)"
  )
  expect_equal(
    as.character(uk_spec("foo", "bar", "baz", comment = "foobar")),
    "CONSTRAINT `bar` UNIQUE KEY `baz` (`foo`) COMMENT 'foobar'"
  )
})

test_that("secondary keys can be specified", {
  expect_error(key_spec())
  expect_error(key_spec("foo", type = "bar"))
  expect_error(key_spec("foo", index_name = 1L))
  expect_error(key_spec("foo", block_size = 1:2))
  expect_s4_class(
    key_spec("foo"),
    "SQL"
  )
  expect_equal(
    as.character(key_spec("foo")),
    "KEY (`foo`)"
  )
  expect_equal(
    as.character(key_spec(c("foo", "bar"))),
    "KEY (`foo`, `bar`)"
  )
  expect_equal(
    as.character(key_spec("fo'o")),
    "KEY (`fo'o`)"
  )
  expect_equal(
    as.character(key_spec("b\nar")),
    "KEY (`b\nar`)"
  )
  expect_equal(
    as.character(key_spec("foo`bar")),
    "KEY (`foo``bar`)"
  )
  expect_equal(
    as.character(key_spec("foo", type = "btree")),
    "KEY USING BTREE (`foo`)"
  )
  expect_equal(
    as.character(key_spec("foo", block_size = 4L)),
    "KEY (`foo`) KEY_BLOCK_SIZE = 4"
  )
  expect_equal(
    as.character(key_spec("foo", comment = "foobar")),
    "KEY (`foo`) COMMENT 'foobar'"
  )
  expect_equal(
    as.character(key_spec("foo", comment = "fo`obar")),
    "KEY (`foo`) COMMENT 'fo`obar'"
  )
  expect_equal(
    as.character(key_spec("foo", comment = "fo\nobar")),
    "KEY (`foo`) COMMENT 'fo\\nobar'"
  )
  expect_equal(
    as.character(key_spec("foo", comment = "fo'obar")),
    "KEY (`foo`) COMMENT 'fo\\'obar'"
  )
  expect_equal(
    as.character(key_spec("foo", "bar")),
    "KEY `bar` (`foo`)"
  )
  expect_equal(
    as.character(key_spec("foo", "bar", comment = "foobar")),
    "KEY `bar` (`foo`) COMMENT 'foobar'"
  )
})

test_that("fulltext keys can be specified", {
  expect_error(ft_key_spec())
  expect_error(ft_key_spec("foo", parser = 1L))
  expect_error(ft_key_spec("foo", index_name = 1L))
  expect_error(ft_key_spec("foo", comment = c("foo", "bar")))
  expect_s4_class(
    ft_key_spec("foo"),
    "SQL"
  )
  expect_equal(
    as.character(ft_key_spec("foo")),
    "FULLTEXT KEY (`foo`)"
  )
  expect_equal(
    as.character(ft_key_spec(c("foo", "bar"))),
    "FULLTEXT KEY (`foo`, `bar`)"
  )
  expect_equal(
    as.character(ft_key_spec("fo'o")),
    "FULLTEXT KEY (`fo'o`)"
  )
  expect_equal(
    as.character(ft_key_spec("b\nar")),
    "FULLTEXT KEY (`b\nar`)"
  )
  expect_equal(
    as.character(ft_key_spec("foo`bar")),
    "FULLTEXT KEY (`foo``bar`)"
  )
  expect_equal(
    as.character(ft_key_spec("foo", parser = "bar")),
    "FULLTEXT KEY (`foo`) WITH PARSER bar"
  )
  expect_equal(
    as.character(ft_key_spec("foo", comment = "foobar")),
    "FULLTEXT KEY (`foo`) COMMENT 'foobar'"
  )
  expect_equal(
    as.character(ft_key_spec("foo", comment = "fo`obar")),
    "FULLTEXT KEY (`foo`) COMMENT 'fo`obar'"
  )
  expect_equal(
    as.character(ft_key_spec("foo", comment = "fo\nobar")),
    "FULLTEXT KEY (`foo`) COMMENT 'fo\\nobar'"
  )
  expect_equal(
    as.character(ft_key_spec("foo", comment = "fo'obar")),
    "FULLTEXT KEY (`foo`) COMMENT 'fo\\'obar'"
  )
  expect_equal(
    as.character(ft_key_spec("foo", "bar")),
    "FULLTEXT KEY `bar` (`foo`)"
  )
  expect_equal(
    as.character(ft_key_spec("foo", "bar", comment = "foobar")),
    "FULLTEXT KEY `bar` (`foo`) COMMENT 'foobar'"
  )
  expect_equal(
    as.character(ft_key_spec("foo", "bar", "baz", "foobar")),
    "FULLTEXT KEY `bar` (`foo`) WITH PARSER baz COMMENT 'foobar'"
  )
})

test_that("spatial keys can be specified", {
  expect_error(spat_key_spec())
  expect_error(spat_key_spec("foo", index_name = 1L))
  expect_error(spat_key_spec("foo", comment = c("foo", "bar")))
  expect_s4_class(
    spat_key_spec("foo"),
    "SQL"
  )
  expect_equal(
    as.character(spat_key_spec("foo")),
    "SPATIAL KEY (`foo`)"
  )
  expect_equal(
    as.character(spat_key_spec(c("foo", "bar"))),
    "SPATIAL KEY (`foo`, `bar`)"
  )
  expect_equal(
    as.character(spat_key_spec("fo'o")),
    "SPATIAL KEY (`fo'o`)"
  )
  expect_equal(
    as.character(spat_key_spec("b\nar")),
    "SPATIAL KEY (`b\nar`)"
  )
  expect_equal(
    as.character(spat_key_spec("foo`bar")),
    "SPATIAL KEY (`foo``bar`)"
  )
  expect_equal(
    as.character(spat_key_spec("foo", comment = "foobar")),
    "SPATIAL KEY (`foo`) COMMENT 'foobar'"
  )
  expect_equal(
    as.character(spat_key_spec("foo", comment = "fo`obar")),
    "SPATIAL KEY (`foo`) COMMENT 'fo`obar'"
  )
  expect_equal(
    as.character(spat_key_spec("foo", comment = "fo\nobar")),
    "SPATIAL KEY (`foo`) COMMENT 'fo\\nobar'"
  )
  expect_equal(
    as.character(spat_key_spec("foo", comment = "fo'obar")),
    "SPATIAL KEY (`foo`) COMMENT 'fo\\'obar'"
  )
  expect_equal(
    as.character(spat_key_spec("foo", "bar")),
    "SPATIAL KEY `bar` (`foo`)"
  )
  expect_equal(
    as.character(spat_key_spec("foo", "bar", comment = "foobar")),
    "SPATIAL KEY `bar` (`foo`) COMMENT 'foobar'"
  )
})

rm_con()
