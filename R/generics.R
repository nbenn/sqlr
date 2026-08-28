#' Render SQL
#'
#' Emits data definition language for `x` in the dialect `dialect`.
#'
#' Rendering a schema is not rendering each table in turn. Foreign keys can
#' form cycles -- a self-reference, or two tables pointing at each other -- so
#' the tables are ordered by dependency and any foreign key that would close a
#' cycle is hoisted out into a trailing `ALTER TABLE`. Rendering a single table
#' emits only what that table can state on its own.
#'
#' @param x Object to render.
#' @param dialect A [sqlr_dialect].
#' @param ... Passed to methods.
#'
#' @return A character vector of statements.
#'
#' @examples
#' # needs a dialect package, e.g.
#' # sqlr_render(schema, sqlr.postgres::postgres())
#'
#' @export
sqlr_render <- new_generic("sqlr_render", c("x", "dialect"))

#' Map between sqlr types and dialect spellings
#'
#' Every dialect owes both directions: reflection has to arrive at the same
#' type object that authoring produced, or an authored schema will never
#' compare equal to the one read back.
#'
#' @param type A [sqlr_type].
#' @param dialect A [sqlr_dialect].
#' @param ... Passed to methods; `sqlr_parse_type()` takes the type spelling
#'   reported by the database catalogue this way.
#'
#' @return `sqlr_render_type()` a string; `sqlr_parse_type()` a [sqlr_type].
#'
#' @export
sqlr_render_type <- new_generic("sqlr_render_type", c("type", "dialect"))

#' @rdname sqlr_render_type
#' @export
sqlr_parse_type <- new_generic("sqlr_parse_type", "dialect")

#' Quote identifiers and literals
#'
#' @param dialect A [sqlr_dialect].
#' @param ... The identifier or literal to quote, passed to methods.
#'
#' @return A string.
#'
#' @export
sqlr_quote <- new_generic("sqlr_quote", "dialect")

#' @rdname sqlr_quote
#' @export
sqlr_quote_literal <- new_generic("sqlr_quote_literal", "dialect")

#' Learn a schema from a database
#'
#' Reads the catalogue of a live database and returns the same representation
#' [sqlr_render()] consumes, so a schema can be rendered, executed and read
#' back for comparison.
#'
#' A dialect must return canonical form: constraint-backed indexes dropped,
#' type spellings collapsed onto [sqlr_type] objects, macro types such as
#' `SERIAL` decomposed, and any casts the engine inserted stripped. Comparison
#' is then plain equality rather than a pile of special cases.
#'
#' @param con A `DBIConnection`.
#' @param schema Schema name to read; `NULL` for the connection default.
#' @param dialect A [sqlr_dialect]; resolved from `con` by default.
#' @param ... Passed to methods.
#'
#' @return A [sqlr_schema()].
#'
#' @export
sqlr_reflect <- function(con, schema = NULL, dialect = sqlr_for(con), ...) {
  sqlr_reflect_schema(dialect, con, schema = schema, ...)
}

#' @rdname sqlr_reflect
#' @export
sqlr_reflect_schema <- new_generic("sqlr_reflect_schema", "dialect")

#' Whether a dialect can add constraints after the fact
#'
#' Governs how foreign key cycles are broken. Where `TRUE`, a foreign key that
#' would close a cycle is hoisted out of `CREATE TABLE` into a trailing
#' `ALTER TABLE ... ADD CONSTRAINT`, and tables are emitted in dependency
#' order. Where `FALSE` -- SQLite being the case in point, since it has no
#' `ADD CONSTRAINT` -- every constraint stays inline and no ordering is
#' attempted, which is safe on engines that permit forward references.
#'
#' @param dialect A [sqlr_dialect].
#' @param ... Passed to methods.
#'
#' @return A flag.
#'
#' @export
sqlr_defers_constraints <- new_generic("sqlr_defers_constraints", "dialect")
