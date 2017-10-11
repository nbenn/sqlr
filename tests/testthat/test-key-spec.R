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