% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db-def.R
\name{create_db_tbl}
\alias{create_db_tbl}
\alias{create_db_tbl.MariaDBConnection}
\title{Generate a database table}
\usage{
create_db_tbl(..., con = get_con())

\method{create_db_tbl}{MariaDBConnection}(..., con = get_con())
}
\arguments{
\item{...}{Arguments passed to the S3 methods; will end up as the arguments
of a call to \code{tbl_spec}}

\item{con}{A connection used to determine the SQL dialect to be used}
}
\value{
A single logical value, indicating whether the statement completed
successfully (invisibly).
}
\description{
All arguments are passed to \code{tbl_spec} which generates
SQL for a CREATE TABLE statement, which in turn is executed.
}
