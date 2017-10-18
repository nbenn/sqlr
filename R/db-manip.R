
#' @title Add a data.frame to database 
#' 
#' @description Add data contained in a \code{data.frame} to database. In case
#' the table does not exist, it will be created by [create_db_tbl], using
#' the arguments passed as \code{...}.
#' 
#' @name write_db_tbl
#' 
#' @param ... Arguments passed to the S3 methods, some of which might end up
#' as arguments to [create_db_tbl].
#' @param con A connection used to determine the SQL dialect to be used
#' 
#' @return The number of added rows (invisibly).
#' 
#' @export
#' 
write_db_tbl <- function(..., con = get_con()) UseMethod("write_db_tbl", con)

#' @param name String specifying the table name
#' @param data A data.frame, holding the data to be added. Columns have to be
#' named.
#' @param mode Either \code{"insert"} or \code{"replace"}, specifying the
#' behavior in case of unique key constraint failures.
#' @param priority One of \code{NA}, \code{"low"}, \code{"high"},
#' \code{"delayed"}. In low priority mode, execution of the \code{INSERT} is
#' delayed until no other clients are reading from the table. High priority
#' mode overrides the effect of the \code{--low-priority-updates} option if the
#' server was started with that option. Both only affect storage engines that
#' use only table-level locking (e.g. MyISAM). Delayed inserts are deprecated.
#' @param ignore Logical switch, specifying whether errors that occur while
#' executing the \code{INSERT} statement are ignored. For example a unique key
#' violation will instead of throwing an error, only throw a warning and
#' discard the offending row to be inserted.
#' @param partition Intended for controlling the behavior of inserts into
#' partitioned tabled, this is currently not implemented.
#' @param on_dupl_key If not \code{NULL} and a row is inserted that causes a
#' cause a unique key constraint failure, an UPDATE of the old row occurs. A
#' named list is expected where names correspond to columns to be updated and
#' list entries are expected to be SQL expressions and of type \code{SQL}.
#' 
#' @rdname write_db_tbl
#' 
#' @section TODO: Also add LOAD DATA INFILE as option for bulk loading
#' 
#' @export
#' 
write_db_tbl.MariaDBConnection <- function(name,
                                           data,
                                           mode = c("insert", "replace"),
                                           priority = c(NA, "low", "high",
                                                        "delayed"),
                                           ignore = FALSE,
                                           partition = NULL,
                                           on_dupl_key = NULL,
                                           ...,
                                           con = get_con()) {

  stopifnot(is.data.frame(data), nrow(data) > 0, ncol(data) > 0,
            !is.null(names(data)), length(names(data)) > 0,
            is_chr(name, n_elem = eq(1L)),
            is_lgl(ignore, n_elem = eq(1L)),
            is_lst(on_dupl_key, names = TRUE, allow_null = TRUE))

  if (!is.null(on_dupl_key))
    stopifnot(all(sapply(on_dupl_key, inherits, "SQL")),
              all(names(on_dupl_key) %in% names(data)))
  if (!is.null(partition)) stop("this is not implemented yet.")

  if (mode == "replace") {
    stopifnot(!is.null(on_dupl_key))
    if (!is.na(priority)) stopifnot(priority == "high")
  }

  priority <- switch(match.arg(priority),
                     low = "LOW_PRIORITY",
                     high = "HIGH_PRIORITY",
                     delayed = "DELAYED")
  mode <- match.arg(mode)

  if (!DBI::dbExistsTable(con, name)) {
    if ("cols" %in% names(list(...)))
      stopifnot(create_db_tbl(name = name, ...))
    else
      stopifnot(create_db_tbl(name = name, cols = get_col_spec(data), ...))
  }

  DBI::dbBegin(con)
  on.exit(DBI::dbRollback(con))

  insert <- DBI::SQL(
    paste0(toupper(mode),
           if (!is.null(priority))
             paste0(" ", priority),
           if (ignore)
             " IGNORE",
           " INTO ", DBI::dbQuoteIdentifier(con, name),
           if (!is.null(partition))
             " PARTITION",
           " (",
             paste0(DBI::dbQuoteIdentifier(con, names(data)),
                    collapse = ", "),
           ") VALUES (",
             paste0(rep("?", ncol(data)), collapse = ", "),
           ")",
           if (!is.null(on_dupl_key))
             paste(" ON DUPLICATE KEY UPDATE",
                   paste(DBI::dbQuoteIdentifier(con, names(on_dupl_key)),
                         on_dupl_key, sep = " = ", collapse = ", "))
    )
  )

  rs <- DBI::dbSendStatement(con, insert)

  rows_added <- tryCatch({
    DBI::dbBind(rs, stats::setNames(data, NULL))
    DBI::dbGetRowsAffected(rs)
  },
    finally = DBI::dbClearResult(rs)
  )

  DBI::dbCommit(con)
  on.exit(NULL)

  invisible(rows_added)
}
