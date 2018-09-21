expect_render <- function(spec, expected) {
  con <- get_con()
  expect_equal(sqlr_render(!!spec, con), DBI::SQL(!!expected))
}

expect_render2 <- function(spec, spec2) {
  con <- get_con()
  expect_equal(sqlr_render(!!spec, con), sqlr_render(!!spec2, con))
}

expect_warning_render <- function(spec, ...) {
  con <- get_con()
  expect_warning(sqlr_render(!!spec, con), ...)
}

expect_error_render <- function(spec, ...) {
  con <- get_con()
  expect_error(sqlr_render(!!spec, con), ...)
}

test_parse_col_spec <- function(x) {
  con <- get_con()
  sql <- sqlr_render(x, con = con)
  parse_col_spec(sql, con = con)
}

test_parse_col_spec_str <- function(x) {
  con <- get_con()
  parse_col_spec(x, con = con)
}

test_parse_data_type <- function(x) {
  con <- get_con()
  sql <- sqlr_render(x, con = con)
  parse_data_type(sql, con = con)
}

test_parse_data_type_str <- function(x) {
  con <- get_con()
  parse_data_type(x, con = con)
}
