context("mysql reference specification")

mysql <- sqlr:::get_con(section = "mysql_unittest")

test_that("references can be specified", {
  expect_error(fk_spec(con = mysql))
  expect_error(fk_spec("foo", "bar", "foobar", con = mysql))
  expect_error(fk_spec("foo", c("bar", "baz"), "foobar", con = mysql))
  expect_s4_class(fk_spec("foo", "bar", "foobar", check = FALSE, con = mysql),
                  "SQL")
  expect_warning(fk_spec("foo", "bar", "foobar", match = "simple",
                         check = FALSE, con = mysql))
  expect_error(fk_spec("foo", "bar", "foobar", match = "xyzzy",
                         check = FALSE, con = mysql))
  expect_equal(as.character(fk_spec("foo", "bar", "foobar", check = FALSE,
                                    con = mysql)),
               paste("FOREIGN KEY (`foo`) REFERENCES `bar` (`foobar`) ON",
                     "DELETE CASCADE ON UPDATE CASCADE"))
  expect_equal(as.character(fk_spec(c("f", "o", "o"), "foobar",
                                    c("b", "a", "r"), check = FALSE,
                                    con = mysql)),
               paste("FOREIGN KEY (`f`, `o`, `o`) REFERENCES `foobar`",
                     "(`b`, `a`, `r`) ON DELETE CASCADE ON UPDATE CASCADE"))
  expect_equal(as.character(fk_spec("foo", "bar", "foobar", "xyzzy",
                                    check = FALSE, con = mysql)),
               paste("CONSTRAINT `xyzzy` FOREIGN KEY (`foo`) REFERENCES `bar`",
                     "(`foobar`) ON DELETE CASCADE ON UPDATE CASCADE"))
  expect_equal(as.character(fk_spec("foo", "bar", "foobar", "xyzzy",
                                    check = FALSE, con = mysql)),
               paste("CONSTRAINT `xyzzy` FOREIGN KEY (`foo`) REFERENCES `bar`",
                     "(`foobar`) ON DELETE CASCADE ON UPDATE CASCADE"))
  expect_equal(as.character(fk_spec("foo", "bar", "baz", "foobar", "xyzzy",
                                    check = FALSE, con = mysql)),
               paste("CONSTRAINT `foobar` FOREIGN KEY `xyzzy` (`foo`)",
                     "REFERENCES `bar` (`baz`) ON DELETE CASCADE ON UPDATE",
                     "CASCADE"))
  expect_equal(as.character(fk_spec("fo'o", "b\nar", "foo`bar", check = FALSE,
                                    con = mysql)),
               paste("FOREIGN KEY (`fo'o`) REFERENCES `b\nar` (`foo``bar`) ON",
                     "DELETE CASCADE ON UPDATE CASCADE"))
  expect_error(fk_spec("foo", "bar", "foobar", on_upd = "xyzzy",
                         check = FALSE, con = mysql))
  expect_equal(as.character(fk_spec("foo", "bar", "foobar",
                                    on_del = "set_null", on_upd = "no_action",
                                    check = FALSE, con = mysql)),
               paste("FOREIGN KEY (`foo`) REFERENCES `bar` (`foobar`) ON",
                     "DELETE SET NULL ON UPDATE NO ACTION"))
})

test_that("primary keys can be specified", {
  expect_error(pk_spec(con = mysql))
  expect_error(pk_spec("foo", type = "bar", con = mysql))
  expect_error(pk_spec("foo", constr_name = 1L, con = mysql))
  expect_error(pk_spec("foo", block_size = 1:2, con = mysql))
  expect_s4_class(pk_spec("foo", con = mysql),
                  "SQL")
  expect_equal(as.character(pk_spec("foo", con = mysql)),
               "PRIMARY KEY (`foo`)")
  expect_equal(as.character(pk_spec(c("foo", "bar"), con = mysql)),
               "PRIMARY KEY (`foo`, `bar`)")
  expect_equal(as.character(pk_spec("fo'o", con = mysql)),
               "PRIMARY KEY (`fo'o`)")
  expect_equal(as.character(pk_spec("b\nar", con = mysql)),
               "PRIMARY KEY (`b\nar`)")
  expect_equal(as.character(pk_spec("foo`bar", con = mysql)),
               "PRIMARY KEY (`foo``bar`)")
  expect_equal(as.character(pk_spec("foo", type = "btree", con = mysql)),
               "PRIMARY KEY USING BTREE (`foo`)")
  expect_equal(as.character(pk_spec("foo", constr_name = "bar", con = mysql)),
               "CONSTRAINT `bar` PRIMARY KEY (`foo`)")
  expect_equal(as.character(pk_spec("foo", constr_name = "b`ar", con = mysql)),
               "CONSTRAINT `b``ar` PRIMARY KEY (`foo`)")
  expect_equal(as.character(pk_spec("foo", block_size = 4L, con = mysql)),
               "PRIMARY KEY (`foo`) KEY_BLOCK_SIZE = 4")
  expect_equal(as.character(pk_spec("foo", comment = "foobar", con = mysql)),
               "PRIMARY KEY (`foo`) COMMENT 'foobar'")
  expect_equal(as.character(pk_spec("foo", comment = "fo`obar", con = mysql)),
               "PRIMARY KEY (`foo`) COMMENT 'fo`obar'")
  expect_equal(as.character(pk_spec("foo", comment = "fo\nobar", con = mysql)),
               "PRIMARY KEY (`foo`) COMMENT 'fo\\nobar'")
  expect_equal(as.character(pk_spec("foo", comment = "fo'obar", con = mysql)),
               "PRIMARY KEY (`foo`) COMMENT 'fo\\'obar'")
})

test_that("unique keys can be specified", {
  expect_error(uk_spec(con = mysql))
  expect_error(uk_spec("foo", type = "bar", con = mysql))
  expect_error(uk_spec("foo", constr_name = 1L, con = mysql))
  expect_error(uk_spec("foo", block_size = 1:2, con = mysql))
  expect_s4_class(uk_spec("foo", con = mysql),
                  "SQL")
  expect_equal(as.character(uk_spec("foo", con = mysql)),
               "UNIQUE KEY (`foo`)")
  expect_equal(as.character(uk_spec(c("foo", "bar"), con = mysql)),
               "UNIQUE KEY (`foo`, `bar`)")
  expect_equal(as.character(uk_spec("fo'o", con = mysql)),
               "UNIQUE KEY (`fo'o`)")
  expect_equal(as.character(uk_spec("b\nar", con = mysql)),
               "UNIQUE KEY (`b\nar`)")
  expect_equal(as.character(uk_spec("foo`bar", con = mysql)),
               "UNIQUE KEY (`foo``bar`)")
  expect_equal(as.character(uk_spec("foo", type = "btree", con = mysql)),
               "UNIQUE KEY USING BTREE (`foo`)")
  expect_equal(as.character(uk_spec("foo", constr_name = "bar", con = mysql)),
               "CONSTRAINT `bar` UNIQUE KEY (`foo`)")
  expect_equal(as.character(uk_spec("foo", constr_name = "b`ar", con = mysql)),
               "CONSTRAINT `b``ar` UNIQUE KEY (`foo`)")
  expect_equal(as.character(uk_spec("foo", block_size = 4L, con = mysql)),
               "UNIQUE KEY (`foo`) KEY_BLOCK_SIZE = 4")
  expect_equal(as.character(uk_spec("foo", comment = "foobar", con = mysql)),
               "UNIQUE KEY (`foo`) COMMENT 'foobar'")
  expect_equal(as.character(uk_spec("foo", comment = "fo`obar", con = mysql)),
               "UNIQUE KEY (`foo`) COMMENT 'fo`obar'")
  expect_equal(as.character(uk_spec("foo", comment = "fo\nobar", con = mysql)),
               "UNIQUE KEY (`foo`) COMMENT 'fo\\nobar'")
  expect_equal(as.character(uk_spec("foo", comment = "fo'obar", con = mysql)),
               "UNIQUE KEY (`foo`) COMMENT 'fo\\'obar'")
  expect_equal(as.character(uk_spec("foo", "bar", "baz", con = mysql)),
               "CONSTRAINT `bar` UNIQUE KEY `baz` (`foo`)")
  expect_equal(as.character(uk_spec("foo", "bar", "baz", comment = "foobar",
                                    con = mysql)),
               "CONSTRAINT `bar` UNIQUE KEY `baz` (`foo`) COMMENT 'foobar'")
})

test_that("secondary keys can be specified", {
  expect_error(key_spec(con = mysql))
  expect_error(key_spec("foo", type = "bar", con = mysql))
  expect_error(key_spec("foo", index_name = 1L, con = mysql))
  expect_error(key_spec("foo", block_size = 1:2, con = mysql))
  expect_s4_class(key_spec("foo", con = mysql),
                  "SQL")
  expect_equal(as.character(key_spec("foo", con = mysql)),
               "KEY (`foo`)")
  expect_equal(as.character(key_spec(c("foo", "bar"), con = mysql)),
               "KEY (`foo`, `bar`)")
  expect_equal(as.character(key_spec("fo'o", con = mysql)),
               "KEY (`fo'o`)")
  expect_equal(as.character(key_spec("b\nar", con = mysql)),
               "KEY (`b\nar`)")
  expect_equal(as.character(key_spec("foo`bar", con = mysql)),
               "KEY (`foo``bar`)")
  expect_equal(as.character(key_spec("foo", type = "btree", con = mysql)),
               "KEY USING BTREE (`foo`)")
  expect_equal(as.character(key_spec("foo", block_size = 4L, con = mysql)),
               "KEY (`foo`) KEY_BLOCK_SIZE = 4")
  expect_equal(as.character(key_spec("foo", comment = "foobar", con = mysql)),
               "KEY (`foo`) COMMENT 'foobar'")
  expect_equal(as.character(key_spec("foo", comment = "fo`obar", con = mysql)),
               "KEY (`foo`) COMMENT 'fo`obar'")
  expect_equal(as.character(key_spec("foo", comment = "fo\nobar",
                                     con = mysql)),
               "KEY (`foo`) COMMENT 'fo\\nobar'")
  expect_equal(as.character(key_spec("foo", comment = "fo'obar", con = mysql)),
               "KEY (`foo`) COMMENT 'fo\\'obar'")
  expect_equal(as.character(key_spec("foo", "bar", con = mysql)),
               "KEY `bar` (`foo`)")
  expect_equal(as.character(key_spec("foo", "bar", comment = "foobar",
                                    con = mysql)),
               "KEY `bar` (`foo`) COMMENT 'foobar'")
})

test_that("fulltext keys can be specified", {
  expect_error(ft_key_spec(con = mysql))
  expect_error(ft_key_spec("foo", parser = 1L, con = mysql))
  expect_error(ft_key_spec("foo", index_name = 1L, con = mysql))
  expect_error(ft_key_spec("foo", comment = c("foo", "bar"), con = mysql))
  expect_s4_class(ft_key_spec("foo", con = mysql),
                  "SQL")
  expect_equal(as.character(ft_key_spec("foo", con = mysql)),
               "FULLTEXT KEY (`foo`)")
  expect_equal(as.character(ft_key_spec(c("foo", "bar"), con = mysql)),
               "FULLTEXT KEY (`foo`, `bar`)")
  expect_equal(as.character(ft_key_spec("fo'o", con = mysql)),
               "FULLTEXT KEY (`fo'o`)")
  expect_equal(as.character(ft_key_spec("b\nar", con = mysql)),
               "FULLTEXT KEY (`b\nar`)")
  expect_equal(as.character(ft_key_spec("foo`bar", con = mysql)),
               "FULLTEXT KEY (`foo``bar`)")
  expect_equal(as.character(ft_key_spec("foo", parser = "bar", con = mysql)),
               "FULLTEXT KEY (`foo`) WITH PARSER bar")
  expect_equal(as.character(ft_key_spec("foo", comment = "foobar",
                                        con = mysql)),
               "FULLTEXT KEY (`foo`) COMMENT 'foobar'")
  expect_equal(as.character(ft_key_spec("foo", comment = "fo`obar",
                                        con = mysql)),
               "FULLTEXT KEY (`foo`) COMMENT 'fo`obar'")
  expect_equal(as.character(ft_key_spec("foo", comment = "fo\nobar",
                                        con = mysql)),
               "FULLTEXT KEY (`foo`) COMMENT 'fo\\nobar'")
  expect_equal(as.character(ft_key_spec("foo", comment = "fo'obar",
                                        con = mysql)),
               "FULLTEXT KEY (`foo`) COMMENT 'fo\\'obar'")
  expect_equal(as.character(ft_key_spec("foo", "bar", con = mysql)),
               "FULLTEXT KEY `bar` (`foo`)")
  expect_equal(as.character(ft_key_spec("foo", "bar", comment = "foobar",
                                        con = mysql)),
               "FULLTEXT KEY `bar` (`foo`) COMMENT 'foobar'")
  expect_equal(as.character(ft_key_spec("foo", "bar", "baz", "foobar",
                                        con = mysql)),
               "FULLTEXT KEY `bar` (`foo`) WITH PARSER baz COMMENT 'foobar'")
})

test_that("spatial keys can be specified", {
  expect_error(spat_key_spec(con = mysql))
  expect_error(spat_key_spec("foo", index_name = 1L, con = mysql))
  expect_error(spat_key_spec("foo", comment = c("foo", "bar"), con = mysql))
  expect_s4_class(spat_key_spec("foo", con = mysql),
                  "SQL")
  expect_equal(as.character(spat_key_spec("foo", con = mysql)),
               "SPATIAL KEY (`foo`)")
  expect_equal(as.character(spat_key_spec(c("foo", "bar"), con = mysql)),
               "SPATIAL KEY (`foo`, `bar`)")
  expect_equal(as.character(spat_key_spec("fo'o", con = mysql)),
               "SPATIAL KEY (`fo'o`)")
  expect_equal(as.character(spat_key_spec("b\nar", con = mysql)),
               "SPATIAL KEY (`b\nar`)")
  expect_equal(as.character(spat_key_spec("foo`bar", con = mysql)),
               "SPATIAL KEY (`foo``bar`)")
  expect_equal(as.character(spat_key_spec("foo", comment = "foobar",
                                          con = mysql)),
               "SPATIAL KEY (`foo`) COMMENT 'foobar'")
  expect_equal(as.character(spat_key_spec("foo", comment = "fo`obar",
                                          con = mysql)),
               "SPATIAL KEY (`foo`) COMMENT 'fo`obar'")
  expect_equal(as.character(spat_key_spec("foo", comment = "fo\nobar",
                                          con = mysql)),
               "SPATIAL KEY (`foo`) COMMENT 'fo\\nobar'")
  expect_equal(as.character(spat_key_spec("foo", comment = "fo'obar",
                                          con = mysql)),
               "SPATIAL KEY (`foo`) COMMENT 'fo\\'obar'")
  expect_equal(as.character(spat_key_spec("foo", "bar", con = mysql)),
               "SPATIAL KEY `bar` (`foo`)")
  expect_equal(as.character(spat_key_spec("foo", "bar", comment = "foobar",
                                          con = mysql)),
               "SPATIAL KEY `bar` (`foo`) COMMENT 'foobar'")
})