extends_type <- function(cls) {
  parent <- cls@parent
  inherits(parent, "S7_class") &&
    (identical(parent, sqlr_type) || extends_type(parent))
}

test_that("the conformance types cover every type class and parameter", {
  ns <- asNamespace("sqlr")
  classes <- Filter(
    function(x) inherits(x, "S7_class") && extends_type(x),
    mget(ls(ns), envir = ns)
  )
  classes$sqlr_other_type <- NULL

  types <- sqlr_conformance_types()
  uncovered <- character()

  for (cls in classes) {
    variants <- Filter(function(x) identical(S7_class(x), cls), types)
    if (!length(variants)) {
      uncovered <- c(uncovered, cls@name)
      next
    }

    default <- cls()
    for (p in setdiff(names(cls@properties), "raw")) {
      varied <- vapply(
        variants,
        function(x) !identical(prop(x, p), prop(default, p)),
        logical(1L)
      )
      if (!any(varied)) uncovered <- c(uncovered, paste0(cls@name, "@", p))
    }
  }

  expect_equal(uncovered, character())
})

test_that("each conformance type has its own name", {
  types <- sqlr_conformance_types()

  expect_false(anyDuplicated(names(types)) > 0L)
  expect_true(all(nzchar(names(types))))
})

test_that("a skipped type must be known and come with a reason", {
  connect <- function() stop("not reached")

  expect_error(
    sqlr_test_types(connect, skip = c(no_such_type = "why")),
    "no_such_type"
  )
  expect_error(sqlr_test_types(connect, skip = c(uuid = "")), "reason")
  expect_error(sqlr_test_types(connect, skip = "why"), "reason")
})

test_that("a connection rather than a way to open one is rejected", {
  expect_error(sqlr_test_types(list()), "is.function")
})
