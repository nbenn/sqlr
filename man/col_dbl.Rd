% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spec-col.R
\name{col_dbl}
\alias{col_dbl}
\alias{col_dbl.MariaDBConnection}
\title{Generate SQL for floating point data type definition}
\usage{
col_dbl(..., con = get_con())

\method{col_dbl}{MariaDBConnection}(prec = "double", unsigned = FALSE, ...)
}
\arguments{
\item{...}{Arguments passed to the S3 methods}

\item{con}{A connection used to determine the SQL dialect to be used}

\item{prec}{One of \code{single}, \code{double}, specifying the floating
point precision the column is expected to be capable of holding.}

\item{unsigned}{Logical switch specifying whether the values are singed or
unsigned.}
}
\value{
SQL to be used in a CREATE table statement
}
\description{
Generate SQL, that can be used for floating point data type
specifications in column definitions for CREATE/ALTER TABLE statements.
}
