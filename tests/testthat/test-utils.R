context("utils")

test_that("argument checking works for vector", {
  expect_true(is_vec(1L))
  expect_true(is_vec(1L, type = "int"))
  expect_true(is_vec(list(), n_elem = NULL))
  expect_true(is_vec(list(), n_elem = gte(0L)))
  expect_false(is_vec(matrix()))
  expect_false(is_vec(NULL))
  expect_true(is_vec(NULL, allow_null = TRUE))
  expect_false(is_vec(NA))
  expect_true(is_vec(NA, allow_na = TRUE))
  expect_error(is_vec(1L, "foo"))
  expect_true(is_vec(1L, NULL))
  expect_error(is_vec(1L, extra_test = "foo"))
  expect_false(is_vec(c(TRUE, FALSE), names = TRUE))
  expect_true(is_vec(setNames(c(TRUE, FALSE), c("a", "b")),
                     names = TRUE))
  expect_true(is_vec(setNames(c(TRUE, FALSE), c("a", "b")),
                     names = c("b", "a")))
  expect_false(is_vec(setNames(c(TRUE, FALSE), c("a", "b")),
                      names = c("a", "c")))
  expect_true(is_vec(setNames(c(TRUE, FALSE), c("a", "b"))))
  expect_error(is_vec(setNames(c(TRUE, FALSE), c("a", "b")), names = NULL))
})

test_that("argument checking works for integer", {
  expect_true(is_int(1L))
  expect_false(is_int(1))
  expect_true(is_int(1, strict = FALSE))
  expect_false(is_int(1.5))
  expect_false(is_int(1.5, strict = FALSE))
  expect_false(is_int(c(1, 2L)))
  expect_true(is_int(c(1, 2L), strict = FALSE))
  expect_false(is_int(c(1, 2L, 2.5), strict = FALSE))
  expect_false(is_int(integer()))
  expect_true(is_int(integer(), n_elem = eq(0L)))
  expect_false(is_int(c(1L, NA)))
  expect_false(is_int(NA, allow_na = TRUE))
  expect_true(is_int(NA_integer_, allow_na = TRUE))
  expect_true(is_int(c(1L, NA), allow_na = TRUE))
  expect_false(is_int(NULL))
  expect_true(is_int(NULL, allow_null = TRUE))
  expect_false(is_int(list(NULL), allow_null = TRUE))
  expect_false(is_int(list(1L)))
  expect_false(is_int(1L, n_elem = eq(2L)))
  expect_true(is_int(c(1L, 2L), n_elem = eq(2L)))
  expect_true(is_int(2147483647L))
  expect_false(is_int(2147483648))
  expect_true(is_int(2147483648, strict = FALSE))
  expect_true(is_int(4294967296, strict = FALSE))
  expect_true(is_int(bit64::as.integer64(2147483648), strict = FALSE))
  expect_true(is_int(bit64::as.integer64(2147483648)))
})

test_that("argument checking works for numeric", {
  expect_true(is_num(1L))
  expect_true(is_num(1))
  expect_true(is_num(1.5))
  expect_false(is_num("1"))
  expect_true(is_num(numeric(), n_elem = eq(0)))
  expect_false(is_num(c(1, NA)))
  expect_false(is_num(NA, allow_na = TRUE))
  expect_true(is_num(NA_integer_, allow_na = TRUE))
  expect_true(is_num(c(1, NA), allow_na = TRUE))
  expect_false(is_num(NULL))
  expect_true(is_num(NULL, allow_null = TRUE))
  expect_false(is_num(list(NULL), allow_null = TRUE))
  expect_false(is_num(list(1)))
  expect_false(is_num(1, n_elem = eq(2)))
  expect_true(is_num(c(1, 2), n_elem = eq(2)))
  expect_false(is_num(c(1, 2), n_elem = gt(2)))
})

test_that("argument checking works for logical", {
  expect_true(is_lgl(TRUE))
  expect_false(is_lgl(integer()))
  expect_false(is_lgl(logical()))
  expect_error(is_lgl(TRUE, type = "int"))
  expect_true(is_lgl(logical(), n_elem = eq(0L)))
  expect_false(is_lgl(c(TRUE, NA)))
  expect_true(is_lgl(NA, allow_na = TRUE))
  expect_true(is_lgl(c(TRUE, NA), allow_na = TRUE))
  expect_false(is_lgl(NULL))
  expect_true(is_lgl(NULL, allow_null = TRUE))
  expect_false(is_lgl(list(NULL), allow_null = TRUE))
  expect_false(is_lgl(list(TRUE)))
  expect_false(is_lgl(TRUE, n_elem = eq(2L)))
  expect_true(is_lgl(c(TRUE, TRUE), n_elem = eq(2L)))
  expect_false(is_lgl(c(TRUE, TRUE), n_elem = gt(2L)))
})

test_that("argument checking works for character", {
  expect_true(is_chr("a"))
  expect_false(is_chr(""))
  expect_false(is_chr(c("a", NA)))
  expect_false(is_chr(NA, allow_na = TRUE))
  expect_true(is_chr(NA_character_, allow_na = TRUE))
  expect_true(is_chr(c("a", NA), allow_na = TRUE))
  expect_false(is_chr(NULL))
  expect_true(is_chr(NULL, allow_null = TRUE))
  expect_false(is_chr(list(NULL), allow_null = TRUE))
  expect_false(is_chr(list("a")))
  expect_true(is_chr("ab"))
  expect_true(is_chr("ab", n_char = gte(2L)))
  expect_false(is_chr("a", n_char = gte(2L)))
  expect_false(is_chr("a", n_char = gt(1L)))
  expect_false(is_chr("ab", n_char = lt(1L)))
  expect_true(is_chr("ab", n_char = lte(2L)))
  expect_false(is_chr("a", n_elem = eq(2L)))
  expect_true(is_chr(c("a", "b"), n_elem = eq(2L)))
  expect_false(is_chr(c("a", "b"), n_elem = gt(2L)))
})

test_that("argument checking works for list", {
  expect_true(is_lst(list(1L)))
  expect_false(is_lst(list(NULL)))
  expect_true(is_lst(list(NULL), allow_null = TRUE))
  expect_true(is_lst(list(a = 1, b = "c")))
  expect_false(is_lst(list(a = 1, b = "c", d = NULL)))
  expect_true(is_lst(list(a = 1, b = "c", d = NULL), allow_null = TRUE))
  expect_false(is_lst(list(a = NA, b = "c", d = NULL), allow_null = TRUE))
  expect_true(is_lst(list(a = NA, b = "c", d = NULL), allow_null = TRUE,
                     allow_na = TRUE))
  expect_false(is_lst(list(a = c(1, NA))))
  expect_true(is_lst(list(a = c(1, NA)), allow_na = TRUE))
  expect_false(is_lst(list(a = list(x = 1, y = NA))))
  expect_true(is_lst(list(a = list(x = 1, y = NA)), allow_na = TRUE))
  expect_true(is_lst(list(a = list(x = 1, y = NULL))))
  expect_true(is_lst(list(a = 1, b = "c"), names = TRUE))
  expect_false(is_lst(list(1, "c"), names = TRUE))
  expect_true(is_lst(list(a = 1, b = "c"), names = c("b", "a")))
  expect_false(is_lst(NULL))
  expect_true(is_lst(NULL, allow_null = TRUE))
  expect_true(is_lst(NULL, names = TRUE, allow_null = TRUE))
  expect_false(is_lst(list(NULL), names = TRUE, allow_null = TRUE))
})
