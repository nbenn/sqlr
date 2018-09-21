expect_render <- function(spec, expected) {
  con <- DBI::dbConnect(RMariaDB::MariaDB())
  on.exit(DBI::dbDisconnect(con))
  expect_equal(sqlr_render(!!spec, con), DBI::SQL(!!expected))
}

expect_render2 <- function(spec, spec2) {
  con <- DBI::dbConnect(RMariaDB::MariaDB())
  on.exit(DBI::dbDisconnect(con))
  expect_equal(sqlr_render(!!spec, con), sqlr_render(!!spec2, con))
}

expect_warning_render <- function(spec, ...) {
  con <- DBI::dbConnect(RMariaDB::MariaDB())
  on.exit(DBI::dbDisconnect(con))
  expect_warning(sqlr_render(!!spec, con), ...)
}

expect_error_render <- function(spec, ...) {
  expect_error(sqlr_render(!!spec, con), ...)
}
