
#' @title Generate SQL for reference definition
#'
#' @description Generate SQL, that can be used for foreign key reference
#' definitions e.g. in CREATE/ALTER TABLE statements.
#'
#' @name fk_spec
#'
#' @param ... Arguments passed on to further methods.
#' @param con Database connection object.
#'
#' @return SQL to be used in a CREATE table statement
#'
#' @section TODO: improve checks and add check unit test as soon as tables can
#' be created.
#'
#' @export
#'
fk_spec <- function(..., con = get_con()) UseMethod("fk_spec", con)

#' @param child_ind Index column name(s) from which the reference is pointing
#' to.
#' @param parent_tbl The table to which the reference is pointing to.
#' @param parent_ind Index column name(s) to which the reference is pointing
#' to.
#' @param constr_name (Optional) name of the constraint.
#' @param index_name (Optional) name of the foreign key.
#' @param match One of \code{NA}, \code{simple}, \code{full}, \code{partial}.
#' For MySQL, NA is recommended.
#' @param on_del,on_upd One of \code{restrict}, \code{cascade},
#' \code{set_null}, \code{no_action}, \code{set_default}, specifying the
#' behavior of \code{UPDATE} and \code{DELETE} operations which affect key
#' values in the parent table that have matching rows in the child table.
#' @param check Logical switch for checking whether parent table/columns are
#' available.
#'
#' @rdname fk_spec
#'
#' @export
#'
fk_spec.MariaDBConnection <- function(child_ind,
                                      parent_tbl,
                                      parent_ind,
                                      constr_name = NA_character_,
                                      index_name = NA_character_,
                                      match = c(
                                        NA, "simple", "full",
                                        "partial"
                                      ),
                                      on_del = "cascade",
                                      on_upd = "cascade",
                                      check = TRUE,
                                      con = get_con(),
                                      ...) {
  stopifnot(
    is_chr(child_ind, n_elem = gte(1L)),
    is_chr(parent_tbl, n_elem = eq(1L)),
    is_chr(parent_ind, n_elem = gte(1L)),
    is_chr(constr_name, n_elem = eq(1L), allow_na = TRUE),
    is_chr(index_name, n_elem = eq(1L), allow_na = TRUE),
    is_chr(on_del, n_elem = eq(1L)),
    is_chr(on_upd, n_elem = eq(1L)),
    is_lgl(check, n_elem = eq(1L))
  )

  match <- match.arg(match)

  if (!is.na(match)) {
    warning(paste(strwrap(
      paste(
        "For MySQL, the use of an explicit MATCH clause will not have the",
        "specified effect, and also causes ON DELETE and ON UPDATE",
        "clauses to be ignored. For these reasons, specifying MATCH",
        "should be avoided."
      ),
      exdent = 2
    ), collapse = "\n"))
  }

  ref_opts <- list(
    restrict = "RESTRICT",
    cascade = "CASCADE",
    set_null = "SET NULL",
    no_action = "NO ACTION",
    set_default = "SET DEFAULT"
  )

  on_del <- match.arg(on_del, names(ref_opts))
  on_upd <- match.arg(on_upd, names(ref_opts))

  if (check) {
    cols <- show_db_cols(parent_tbl, con = con)
    stopifnot(all(parent_ind %in% cols$Field))
    # also check that
    # - referenced cols are unique
    # - referenced cols are not null
    # - same no of cols and type as child_ind
  }

  DBI::SQL(paste0(
    if (!is.na(constr_name))
      paste0(
        "CONSTRAINT ",
        DBI::dbQuoteIdentifier(con, constr_name),
        " "
      ),
    "FOREIGN KEY",
    if (!is.na(index_name))
      paste0(" ", DBI::dbQuoteIdentifier(con, index_name)),
    " (",
    paste(DBI::dbQuoteIdentifier(con, child_ind),
      collapse = ", "
    ),
    ")",
    " REFERENCES ",
    DBI::dbQuoteIdentifier(con, parent_tbl),
    " (",
    paste(DBI::dbQuoteIdentifier(con, parent_ind),
      collapse = ", "
    ),
    ")",
    if (!is.na(match))
      paste("MATCH", toupper(match)),
    " ON DELETE ", ref_opts[[on_del]],
    " ON UPDATE ", ref_opts[[on_upd]]
  ))
}


#' @title Generate SQL for primary key definition
#'
#' @description Generate SQL, that can be used for primary key definitions
#' e.g. in CREATE/ALTER TABLE statements.
#'
#' @name pk_spec
#'
#' @param ... Arguments passed on to further methods.
#' @param con Database connection object.
#'
#' @return SQL to be used in a CREATE table statement
#'
#' @export
#'
pk_spec <- function(..., con = get_con()) UseMethod("pk_spec", con)

#' @param cols Character vector specifying the column(s) to be used for the
#' key
#' @param constr_name (Optional) name of the constraint.
#' @param type One of \code{NA}, \code{btree}, \code{hash}, specifying the
#' type of the index. NA causes the USING clause to be omitted which will
#' yield the default for the given storage engine, most likely BTREE.
#' @param block_size An integer value that optionally specifies the size in
#' bytes to use for index key blocks (MyISAM). For InnoDB, only A table-level
#' KEY_BLOCK_SIZE value is permitted.
#' @param comment Index definitions can include an optional comment of up to
#' 1024 characters.
#'
#' @rdname pk_spec
#'
#' @export
#'
pk_spec.MariaDBConnection <- function(cols,
                                      constr_name = NA_character_,
                                      type = c(NA, "btree", "hash"),
                                      block_size = NA_integer_,
                                      comment = NA_character_,
                                      con = get_con(),
                                      ...) {
  stopifnot(
    is_chr(cols, n_elem = gte(1L)),
    is_chr(constr_name, n_elem = eq(1L), allow_na = TRUE),
    is_int(block_size,
      n_elem = eq(1L), strict = FALSE,
      allow_na = TRUE
    ),
    is_chr(comment, n_elem = eq(1L), allow_na = TRUE)
  )

  type <- match.arg(type)

  DBI::SQL(paste0(
    if (!is.na(constr_name))
      paste0(
        "CONSTRAINT ",
        DBI::dbQuoteIdentifier(con, constr_name),
        " "
      ),
    "PRIMARY KEY",
    if (!is.na(type))
      paste0(" USING ", toupper(type)),
    " (",
    paste(DBI::dbQuoteIdentifier(con, cols),
      collapse = ", "
    ),
    ")",
    if (!is.na(block_size))
      paste0(" KEY_BLOCK_SIZE = ", block_size),
    if (!is.na(comment))
      paste0(" COMMENT ", DBI::dbQuoteString(con, comment))
  ))
}

#' @title Generate SQL for unique key definition
#'
#' @description Generate SQL, that can be used for unique key definitions
#' e.g. in CREATE/ALTER TABLE statements.
#'
#' @name uk_spec
#'
#' @param ... Arguments passed on to further methods.
#' @param con Database connection object.
#'
#' @return SQL to be used in a CREATE table statement
#'
#' @export
#'
uk_spec <- function(..., con = get_con()) UseMethod("uk_spec", con)

#' @inheritParams pk_spec
#' @param index_name (Optional) name of the index.
#'
#' @rdname uk_spec
#'
#' @export
#'
uk_spec.MariaDBConnection <- function(cols,
                                      constr_name = NA_character_,
                                      index_name = NA_character_,
                                      type = c(NA, "btree", "hash"),
                                      block_size = NA_integer_,
                                      comment = NA_character_,
                                      con = get_con(),
                                      ...) {
  stopifnot(
    is_chr(cols, n_elem = gte(1L)),
    is_chr(constr_name, n_elem = eq(1L), allow_na = TRUE),
    is_chr(index_name, n_elem = eq(1L), allow_na = TRUE),
    is_int(block_size, n_elem = eq(1L), allow_na = TRUE),
    is_chr(comment, n_elem = eq(1L), allow_na = TRUE)
  )

  type <- match.arg(type)

  DBI::SQL(paste0(
    if (!is.na(constr_name))
      paste0(
        "CONSTRAINT ",
        DBI::dbQuoteIdentifier(con, constr_name),
        " "
      ),
    "UNIQUE KEY",
    if (!is.na(index_name))
      paste0(" ", DBI::dbQuoteIdentifier(con, index_name)),
    if (!is.na(type))
      paste0(" USING ", toupper(type)),
    " (",
    paste(DBI::dbQuoteIdentifier(con, cols),
      collapse = ", "
    ),
    ")",
    if (!is.na(block_size))
      paste0(" KEY_BLOCK_SIZE = ", block_size),
    if (!is.na(comment))
      paste0(" COMMENT ", DBI::dbQuoteString(con, comment))
  ))
}

#' @title Generate SQL for (secondary) key definition
#'
#' @description Generate SQL, that can be used for (secondary) key definitions
#' e.g. in CREATE/ALTER TABLE statements.
#'
#' @name key_spec
#'
#' @param ... Arguments passed on to further methods.
#' @param con Database connection object.
#'
#' @return SQL to be used in a CREATE table statement
#'
#' @export
#'
key_spec <- function(..., con = get_con()) UseMethod("key_spec", con)

#' @inheritParams uk_spec
#'
#' @rdname key_spec
#'
#' @export
#'
key_spec.MariaDBConnection <- function(cols,
                                       index_name = NA_character_,
                                       type = c(NA, "btree", "hash"),
                                       block_size = NA_integer_,
                                       comment = NA_character_,
                                       con = get_con(),
                                       ...) {
  stopifnot(
    is_chr(cols, n_elem = gte(1L)),
    is_chr(index_name, n_elem = eq(1L), allow_na = TRUE),
    is_int(block_size, n_elem = eq(1L), allow_na = TRUE),
    is_chr(comment, n_elem = eq(1L), allow_na = TRUE)
  )

  type <- match.arg(type)

  DBI::SQL(paste0(
    "KEY",
    if (!is.na(index_name))
      paste0(" ", DBI::dbQuoteIdentifier(con, index_name)),
    if (!is.na(type))
      paste0(" USING ", toupper(type)),
    " (",
    paste(DBI::dbQuoteIdentifier(con, cols),
      collapse = ", "
    ),
    ")",
    if (!is.na(block_size))
      paste0(" KEY_BLOCK_SIZE = ", block_size),
    if (!is.na(comment))
      paste0(" COMMENT ", DBI::dbQuoteString(con, comment))
  ))
}

#' @title Generate SQL for fulltext key definition
#'
#' @description Generate SQL, that can be used for fulltext key definitions
#' e.g. in CREATE/ALTER TABLE statements.
#'
#' @name ft_key_spec
#'
#' @param ... Arguments passed on to further methods.
#' @param con Database connection object.
#'
#' @return SQL to be used in a CREATE table statement
#'
#' @section TODO: implement a function show_db_plugins that fetches SHOW
#' PLUGINS and use the results for checking if the parser is listed as
#' FTPARSER. Currently the parser is passed without being escaped!
#'
#' @export
#'
ft_key_spec <- function(..., con = get_con()) UseMethod("ft_key_spec", con)

#' @inheritParams uk_spec
#' @param parser Name of the full-text parser to be used in the index. Examples
#' are \code{ngram}
#' (\url{https://dev.mysql.com/doc/refman/5.7/en/fulltext-search-ngram.html})
#' and \code{mecab}
#' (\url{https://dev.mysql.com/doc/refman/5.7/en/fulltext-search-mecab.html}).
#'
#' @rdname ft_key_spec
#'
#' @export
#'
ft_key_spec.MariaDBConnection <- function(cols,
                                          index_name = NA_character_,
                                          parser = NA_character_,
                                          comment = NA_character_,
                                          con = get_con(),
                                          ...) {
  stopifnot(
    is_chr(cols, n_elem = gte(1L)),
    is_chr(index_name, n_elem = eq(1L), allow_na = TRUE),
    is_chr(parser, n_elem = eq(1L), allow_na = TRUE),
    is_chr(comment, n_elem = eq(1L), allow_na = TRUE)
  )

  DBI::SQL(paste0(
    "FULLTEXT KEY",
    if (!is.na(index_name))
      paste0(" ", DBI::dbQuoteIdentifier(con, index_name)),
    " (",
    paste(DBI::dbQuoteIdentifier(con, cols),
      collapse = ", "
    ),
    ")",
    if (!is.na(parser))
      paste0(" WITH PARSER ", parser),
    if (!is.na(comment))
      paste0(" COMMENT ", DBI::dbQuoteString(con, comment))
  ))
}

#' @title Generate SQL for spatial key definition
#'
#' @description Generate SQL, that can be used for spatial key definitions
#' e.g. in CREATE/ALTER TABLE statements.
#'
#' @name spat_key_spec
#'
#' @param ... Arguments passed on to further methods.
#' @param con Database connection object.
#'
#' @return SQL to be used in a CREATE table statement
#'
#' @export
#'
spat_key_spec <- function(..., con = get_con()) UseMethod("spat_key_spec", con)

#' @inheritParams uk_spec
#'
#' @rdname spat_key_spec
#'
#' @export
#'
spat_key_spec.MariaDBConnection <- function(cols,
                                            index_name = NA_character_,
                                            comment = NA_character_,
                                            con = get_con(),
                                            ...) {
  stopifnot(
    is_chr(cols, n_elem = gte(1L)),
    is_chr(index_name, n_elem = eq(1L), allow_na = TRUE),
    is_chr(comment, n_elem = eq(1L), allow_na = TRUE)
  )

  DBI::SQL(paste0(
    "SPATIAL KEY",
    if (!is.na(index_name))
      paste0(" ", DBI::dbQuoteIdentifier(con, index_name)),
    " (",
    paste(DBI::dbQuoteIdentifier(con, cols),
      collapse = ", "
    ),
    ")",
    if (!is.na(comment))
      paste0(" COMMENT ", DBI::dbQuoteString(con, comment))
  ))
}
