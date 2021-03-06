% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-con.R
\name{load_config}
\alias{load_config}
\title{Load yml configuration file}
\usage{
load_config(file_name = NULL, section = NULL)
}
\arguments{
\item{file_name}{The file name of the config file. If no value is supplied,
the current wd is searched for files ending in '.yaml' or '.yml', followed
by the extdata dir in the package installation.}

\item{section}{A list node to be returned instead of the whole config list.
If multiple nodes match, the last match (the list is traversed recursively)
will be returned}
}
\value{
The database configuration as a list.
}
\description{
Load the yml configuration file used for connecting to a data
base.
}
