\name{getColumns}
\alias{getColumns}
\title{Helper Function}
\usage{
  getColumns(tableInfo, types, names.only, include, except)
}
\arguments{
  \item{tableInfo}{data frame obtained by calling
  \code{\link{getTableSummary}} or
  \code{\link{sqlColumns}}.}

  \item{types}{vector with data types to select}

  \item{names.only}{logical: if TRUE returns column names
  only, otherwise full rows of \code{tableInfo}.}

  \item{include}{a vector of column names to include.
  Output is restricted to this list.}

  \item{except}{a vector of column names to exclude. Output
  never contains names from this list.}
}
\description{
  Helper Function
}

