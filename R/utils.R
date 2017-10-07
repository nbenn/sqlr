
#' @title Config environment
#' 
#' @description Configuration information, as well as the current connection
#' is save to and fetched from this environment.
#' 
config <- new.env()

#' @title Load yml configuration file
#' 
#' @param file_name The file name of the config file. If no value is supplied,
#' the current wd is searched for files ending in '.yaml' or '.yml', followed
#' by the extdata dir in the package installation.
#' 
#' @return The database configuration as a list.
#'
load_config <- function(file_name = NULL) {

  find_yml <- function(dir) {
    list.files(dir, pattern = "\\.y[a]?ml$", full.names = TRUE)[1]
  }

  if (is.null(file_name)) {

    file_name <- find_yml(getwd())

    if (is.na(file_name)) {
      file_name <- find_yml(system.file("extdata",
                                        package = methods::getPackageName()))
    }

    stopifnot(!is.na(file_name))

  } else stopifnot(file.exists(file_name))

  yaml::yaml.load_file(file_name)
}

#' @title Load config list
#' 
#' @inheritParams load_config
#' @param update Logical, whether to force reading the config again from file.
#' @param section Name of the top-level config file section to return
#' 
#' @return The database configuration as a list.
#'
get_cfg <- function(file_name = NULL,
                    update = FALSE,
                    section = NULL) {

  if (is.null(config$cfg) || update)
    config$cfg <- load_config(file_name)

  if (!is.null(section)) config$cfg[[section]]
  else config$cfg
}

#' @title Get the database connection
#' 
#' @inheritParams load_config
#' @param update Logical, whether to force destroying and recreating the
#' database connection.
#' 
#' @return The database connection object.
#'
get_con <- function(cfg_file = NULL,
                    update = FALSE) {

  if (is.null(config$con) || !DBI::dbIsValid(config$con) || update)
    config$con <- connect_db(get_cfg(cfg_file, section = "db_setup"))

  config$con
}

#' @title Destroy the database connection
#' 
#' @return NULL (invisibly)
#'
rm_con <- function() {

  if (!is.null(config$con))
    config$con <- NULL

  invisible(NULL)
}


#' @title Connect to a database
#'
#' @description Depending on the type selected by the user, the appropriate
#' function is called to set up and/or connect to a database.
#' 
#' @param dbType The type of database to connect to (default: "mysql")
#' 
#' @return A connection object that can be used for further database access.
#' 
connect_db <- function(config) {
  db_type <- match.arg(config$db_type, "mysql")
  switch(db_type,
         mysql = do.call(connect_mysql, config))
}

#' @title Connect to a MySQL database
#'
#' @description This function provides a wrapper around \code{dbConnect} that
#' sets up a database and user with all privileges to that schema if any of the
#' two does not already exist.
#' 
#' @param db_name The name of the database to connect to/create
#' @param user The username with which to connect to the database
#' @param password The password used to authenticate the user with the
#' database
#' @param host Hostname of the MySQL server (default: "localhost")
#' @param port The port of the MySQL server (default: 3306)
#' 
#' @return Connection object that can be used for further database access.
#' 
connect_mysql <- function(db_name,
                          username,
                          password,
                          host = "localhost",
                          port = 3306,
                          ...) {

  con <- tryCatch({
    DBI::dbConnect(RMariaDB::MariaDB(), username = username,
                   password = password, host = host, port = port,
                   dbname = db_name)
    },
    error = function(e) {
      message("in order to set up the database, the credentials of an ",
              "account with CREATE\nand GRANT privileges are needed ",
              "temporarily.\nPlease enter the username (default root): ",
              appendLF = FALSE)

      root_user <- readline()
      if (root_user == "")
        root_user <- "root"

      message("Please enter the password: ", appendLF = FALSE)
      if (requireNamespace("getPass", quietly = TRUE))
        root_pwd <- getPass::getPass("")
      else
        root_pwd <- readline()

      tmp <- DBI::dbConnect(RMariaDB::MariaDB(), username = root_user,
                            password = root_pwd, host = host, port = port)
      on.exit(DBI::dbDisconnect(tmp))

      invisible(DBI::dbExecute(
        tmp,
        DBI::SQL(paste("CREATE DATABASE IF NOT EXISTS",
                       DBI::dbQuoteIdentifier(tmp, db_name)))))

      invisible(DBI::dbExecute(
        tmp,
        DBI::SQL(paste0("GRANT ALL ON ", DBI::dbQuoteIdentifier(tmp, db_name),
                        ".* TO ",
                        DBI::dbQuoteString(tmp, username), "@",
                        DBI::dbQuoteString(tmp, host),
                        " IDENTIFIED BY ",
                        DBI::dbQuoteString(tmp, password)))))

      DBI::dbConnect(RMariaDB::MariaDB(), username = username,
                     password = password, host = host, port = port,
                     dbname = db_name)
    }
  )

  attr(con, "finaliser") <- (function(con) {
    reg.finalizer(environment(), function(...) {
      message("Auto-disconnecting ", class(con)[[1]])
      DBI::dbDisconnect(con)
    })
    environment()
  })(con)

  con
}
