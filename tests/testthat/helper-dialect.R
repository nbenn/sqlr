library(S7)

test_dialect <- new_class("test_dialect", parent = sqlr_dialect)

method(sqlr_render_type, list(sqlr_type, test_dialect)) <-
  function(type, dialect, ...) "TYPE"

method(sqlr_render_type, list(sqlr_integer_type, test_dialect)) <-
  function(type, dialect, ...) paste0("INT", type@bytes)

method(sqlr_render_type, list(sqlr_string_type, test_dialect)) <-
  function(type, dialect, ...) {
    if (is.na(type@size)) "TEXT" else paste0("VARCHAR(", type@size, ")")
  }
