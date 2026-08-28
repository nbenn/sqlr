test_that("a table renders its columns and constraints", {
  out <- sqlr_render(
    sqlr_table(
      "users",
      sqlr_column("id", sqlr_bigint()),
      sqlr_column("email", "varchar(255)", null = FALSE),
      sqlr_primary_key("id", name = "users_pkey")
    ),
    test_dialect()
  )

  expect_length(out, 1L)
  expect_match(out, "CREATE TABLE \"users\"")
  expect_match(out, "\"id\" INT8 NOT NULL")
  expect_match(out, "\"email\" VARCHAR\\(255\\) NOT NULL")
  expect_match(out, "CONSTRAINT \"users_pkey\" PRIMARY KEY \\(\"id\"\\)")
})

test_that("tables are ordered so referenced tables come first", {
  schema <- sqlr_schema(
    "s",
    sqlr_table(
      "orders",
      sqlr_column("id", sqlr_int()),
      sqlr_column("user_id", sqlr_int()),
      sqlr_foreign_key("user_id", "users", "id")
    ),
    sqlr_table("users", sqlr_column("id", sqlr_int()), sqlr_primary_key("id"))
  )

  out <- sqlr_render(schema, test_dialect())

  expect_lt(grep("CREATE TABLE \"s\".\"users\"", out), grep("CREATE TABLE \"s\".\"orders\"", out))
})

test_that("a self-referencing foreign key is hoisted out of CREATE TABLE", {
  schema <- sqlr_schema(
    "s",
    sqlr_table(
      "employees",
      sqlr_column("id", sqlr_int()),
      sqlr_column("manager_id", sqlr_int()),
      sqlr_primary_key("id"),
      sqlr_foreign_key("manager_id", "employees", "id", name = "mgr_fk")
    )
  )

  out <- sqlr_render(schema, test_dialect())

  expect_length(out, 2L)
  expect_false(grepl("FOREIGN KEY", out[[1L]]))
  expect_match(out[[2L]], "^ALTER TABLE \"s\".\"employees\" ADD CONSTRAINT \"mgr_fk\"")
})

test_that("mutually referencing tables still render", {
  schema <- sqlr_schema(
    "s",
    sqlr_table(
      "a",
      sqlr_column("id", sqlr_int()),
      sqlr_column("b_id", sqlr_int()),
      sqlr_foreign_key("b_id", "b", "id")
    ),
    sqlr_table(
      "b",
      sqlr_column("id", sqlr_int()),
      sqlr_column("a_id", sqlr_int()),
      sqlr_foreign_key("a_id", "a", "id")
    )
  )

  out <- sqlr_render(schema, test_dialect())

  expect_length(grep("^CREATE TABLE", out), 2L)
  expect_length(grep("^ALTER TABLE", out), 2L)
  expect_false(any(grepl("FOREIGN KEY", grep("^CREATE TABLE", out, value = TRUE))))
})

test_that("foreign keys inherit the schema qualifier", {
  schema <- sqlr_schema(
    "app",
    sqlr_table("users", sqlr_column("id", sqlr_int()), sqlr_primary_key("id")),
    sqlr_table(
      "orders",
      sqlr_column("id", sqlr_int()),
      sqlr_column("user_id", sqlr_int()),
      sqlr_foreign_key("user_id", "users", "id")
    )
  )

  out <- sqlr_render(schema, test_dialect())

  expect_match(paste(out, collapse = "\n"), "REFERENCES \"app\".\"users\"")
})

test_that("defaults and identity render", {
  out <- sqlr_render(
    sqlr_table(
      "t",
      sqlr_column("id", sqlr_bigint(), identity = sqlr_identity("always")),
      sqlr_column("label", sqlr_text(), default = "none"),
      sqlr_column("at", sqlr_text(), default = sqlr_sql("now()"))
    ),
    test_dialect()
  )

  expect_match(out, "GENERATED ALWAYS AS IDENTITY")
  expect_match(out, "DEFAULT 'none'")
  expect_match(out, "DEFAULT now\\(\\)")
})
