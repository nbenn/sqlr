% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spec-key.R
\name{pk_spec}
\alias{pk_spec}
\alias{pk_spec.MariaDBConnection}
\title{Generate SQL for primary key definition}
\usage{
pk_spec(..., con = get_con())

\method{pk_spec}{MariaDBConnection}(cols, constr_name = NA_character_,
  type = c(NA, "btree", "hash"), block_size = NA_integer_,
  comment = NA_character_, con = get_con(), ...)
}
\arguments{
\item{...}{Arguments passed on to further methods.}

\item{con}{Database connection object.}

\item{cols}{Character vector specifying the column(s) to be used for the
key}

\item{constr_name}{(Optional) name of the constraint.}

\item{type}{One of \code{NA}, \code{btree}, \code{hash}, specifying the
type of the index. NA causes the USING clause to be omitted which will
yield the default for the given storage engine, most likely BTREE.}

\item{block_size}{An integer value that optionally specifies the size in
bytes to use for index key blocks (MyISAM). For InnoDB, only A table-level
KEY_BLOCK_SIZE value is permitted.}

\item{comment}{Index definitions can include an optional comment of up to
1024 characters.}
}
\value{
SQL to be used in a CREATE table statement
}
\description{
Generate SQL, that can be used for primary key definitions
e.g. in CREATE/ALTER TABLE statements.
}
