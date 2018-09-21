
#' @title Config environment
#'
#' @description Configuration information, as well as the current connection
#' is saved to and fetched from this environment.
#'
config <- new.env()

#' @title Load yml configuration file
#'
#' @description Load the yml configuration file used for connecting to a data
#' base.
#'
#' @param file_name The file name of the config file. If no value is supplied,
#' the current wd is searched for files ending in '.yaml' or '.yml', followed
#' by the extdata dir in the package installation.
#' @param section A list node to be returned instead of the whole config list.
#' If multiple nodes match, the last match (the list is traversed recursively)
#' will be returned
#'
#' @return The database configuration as a list.
#'
load_config <- function(file_name = NULL,
                        section = NULL) {
  find_yml <- function(dir) {
    list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)[1]
  }

  find_section <- function(lst, sec) {
    if (!is.list(lst)) {
      NULL
    } else if (sec %in% names(lst)) {
      section <<- lst[[sec]]
    } else {
      lapply(lst, find_section, sec)
    }
  }

  stopifnot(
    is_chr(file_name, n_elem = eq(1L), allow_null = TRUE),
    is_chr(section, n_elem = eq(1L), allow_null = TRUE)
  )

  if (is.null(file_name)) {
    file_name <- find_yml(getwd())

    if (is.na(file_name)) {
      file_name <- find_yml(system.file("extdata",
        package = methods::getPackageName()
      ))
    }

    stopifnot(!is.na(file_name))
  } else {
    stopifnot(file.exists(file_name))
  }

  cfg <- yaml::yaml.load_file(file_name)

  if (!is.null(section)) {
    if (is.null(unlist(find_section(cfg, section)))) {
      stop("section ", section, " not found in ", file_name)
    }
    section
  } else {
    cfg
  }
}

#' @title Set the database connection
#'
#' @description Using the information from the yml config file, generate a
#' data base connection object and store it alongside the config information
#' in the config environment.
#'
#' @inheritParams load_config
#' @param update Logical switch for forcing a reset to the con object despite
#' it being present and valid.
#'
#' @return TRUE if the con object was set, FALSE if not.
#'
#' @export
#'
set_con <- function(file_name = NULL,
                    section = "db_setup",
                    update = FALSE) {
  stopifnot(is_lgl(update, n_elem = eq(1L)))

  if (!is.null(config$con) && (!DBI::dbIsValid(config$con) || update)) {
    rm_con()
  }

  if (is.null(config$con)) {
    config$con <- connect_db(load_config(file_name, section))
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
}

#' @title Get the database connection
#'
#' @description Fetch the database connection stored in the config environment.
#' If no valid con object is present, it will be created using [set_con()].
#'
#' @param ... Arguments passed to [set_con].
#'
#' @return The database connection object.
#'
get_con <- function(...) {
  set_con(...)
  config$con
}

#' @title Destroy the database connection
#'
#' @description Destroy the database connection object that is currently saved
#' in the config environment.
#'
#' @return NULL (invisibly)
#'
#' @export
#'
rm_con <- function() {
  if (!is.null(config$con)) DBI::dbDisconnect(config$con)
  config$con <- NULL
  invisible(NULL)
}


#' @title Connect to a database
#'
#' @description Depending on the type selected by the user, the appropriate
#' function is called to set up and/or connect to a database.
#'
#' @param config A named list containing parameters for \code{dbConnect}, as
#' well as a slot "dbtype", specifying what database system to connect to.
#'
#' @return A connection object that can be used for further database access.
#'
connect_db <- function(config) {
  dbtype <- match.arg(config$dbtype, "mysql")

  switch(dbtype,
    mysql = do.call(connect_mysql, config[names(config) != "dbtype"])
  )
}

#' @title Connect to a MySQL database
#'
#' @description This function provides a wrapper around \code{dbConnect} that
#' sets up a database and user with all privileges to that schema if any of the
#' two does not already exist.
#'
#' @param ... Arguments passed to \code{dbConnect}
#'
#' @return Connection object that can be used for further database access. The
#' connection is automatically destroyed when garbage collected.
#'
connect_mysql <- function(...) {
  dots <- list(...)
  if (is.null(dots$host)) dots$host <- "localhost"

  con <- tryCatch(
    do.call(DBI::dbConnect, c(RMariaDB::MariaDB(), dots)),
    error = function(e) {
      # nocov start
      if (interactive()) {
        message(paste(strwrap(
          paste0(
            "in order to set up the database ", dots$dbname, ", the ",
            "credentials of an account with CREATE and GRANT privileges ",
            "are needed temporarily. Please enter the username (default ",
            "root): "
          )
        ), collapse = "\n"), appendLF = FALSE)

        root_dots <- dots[names(dots) != "dbname"]

        root_dots$username <- readline()
        if (root_dots$username == "") {
          root_dots$username <- "root"
        }

        message("Please enter the password: ", appendLF = FALSE)
        root_dots$password <- readline()

        root_con <- do.call(DBI::dbConnect, c(RMariaDB::MariaDB(), root_dots))
        on.exit(DBI::dbDisconnect(root_con))

        invisible(DBI::dbExecute(
          root_con,
          DBI::SQL(paste(
            "CREATE DATABASE IF NOT EXISTS",
            DBI::dbQuoteIdentifier(root_con, dots$dbname)
          ))
        ))

        invisible(DBI::dbExecute(
          root_con,
          DBI::SQL(paste0(
            "GRANT ALL ON ", DBI::dbQuoteIdentifier(
              root_con,
              dots$dbname
            ),
            ".* TO ",
            DBI::dbQuoteString(root_con, dots$username), "@",
            DBI::dbQuoteString(root_con, dots$host),
            if (!is.null(dots$password)) {
              paste(
                " IDENTIFIED BY",
                DBI::dbQuoteString(root_con, dots$password)
              )
            }
          ))
        ))
        do.call(DBI::dbConnect, c(RMariaDB::MariaDB(), dots))
      } else {
        stop(e)
      }
      # nocov end
    }
  )

  con
}
