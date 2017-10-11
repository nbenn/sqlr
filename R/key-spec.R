
#' @title Generate SQL for reference definition
#' 
#' @description Generate SQL, that can be used for foreign key reference
#' definitions e.g. in CREATE/ALTER TABLE statements.
#' 
#' @param child_ind Index column name(s) from which the reference is pointing
#' to.
#' @param parent_tbl The table to which the reference is pointing to.
#' @param parent_ind Index column name(s) to which the reference is pointing
#' to.
#' @param constr_name (Optional) name of the constraint.
#' @param fk_name (Optional) name of the foreign key.
#' @param match One of \code{NA}, \code{simple}, \code{full}, \code{partial}.
#' For MySQL, NA is recommended.
#' @param on_del,on_upd One of \code{restrict}, \code{cascade},
#' \code{set_null}, \code{no_action}, \code{set_default}, specifying the
#' behavior of \code{UPDATE} and \code{DELETE} operations which affect key
#' values in the parent table that have matching rows in the child table.
#' @param check Logical switch for checking whether parent table/columns are
#' available.
#' 
#' @return SQL to be used in a CREATE table statement
#' 
#' @section TODO: improve checks and add check unit test as soon as tables can
#' be created.
#' 
#' @export
#' 
fk_spec <- function(..., con = get_con()) UseMethod("fk_spec", con)

#' @export
fk_spec.MariaDBConnection <- function(child_ind,
                                      parent_tbl,
                                      parent_ind,
                                      constr_name = NA_character_,
                                      fk_name = NA_character_,
                                      match = c(NA, "simple", "full",
                                                "partial"),
                                      on_del = "cascade",
                                      on_upd = "cascade",
                                      check = TRUE,
                                      con = get_con(),
                                      ...) {

  stopifnot(is.character(child_ind), length(child_ind) >= 1,
            is.character(parent_tbl), length(parent_tbl) == 1,
            is.character(parent_ind), length(parent_ind) >= 1,
            is.character(constr_name), length(constr_name) == 1,
            is.character(fk_name), length(fk_name) == 1,
            is.character(on_del), length(on_del) == 1,
            is.character(on_upd), length(on_upd) == 1,
            is.logical(check), length(check) == 1)

  match <- match.arg(match)

  if (!is.na(match)) {
    warning(paste(strwrap(
      paste("For MySQL, the use of an explicit MATCH clause will not have the",
            "specified effect, and also causes ON DELETE and ON UPDATE",
            "clauses to be ignored. For these reasons, specifying MATCH",
            "should be avoided."), exdent = 2), collapse = "\n"))
  }

  ref_opts <- list(restrict = "RESTRICT",
                   cascade = "CASCADE",
                   set_null = "SET NULL",
                   no_action = "NO ACTION",
                   set_default = "SET DEFAULT")

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

  DBI::SQL(paste0(if (!is.na(constr_name))
                    paste0("CONSTRAINT ",
                           DBI::dbQuoteIdentifier(con, constr_name),
                           " "),
                  "FOREIGN KEY",
                  if (!is.na(fk_name))
                    paste0(" ", DBI::dbQuoteIdentifier(con, fk_name)),
                  " (",
                    paste(DBI::dbQuoteIdentifier(con, child_ind),
                          collapse = ", "),
                  ")",
                  " REFERENCES ",
                  DBI::dbQuoteIdentifier(con, parent_tbl),
                  " (",
                    paste(DBI::dbQuoteIdentifier(con, parent_ind),
                          collapse = ", "),
                  ")",
                  if (!is.na(match))
                    paste("MATCH", toupper(match)),
                  " ON DELETE ", ref_opts[[on_del]],
                  " ON UPDATE ", ref_opts[[on_upd]]))
}