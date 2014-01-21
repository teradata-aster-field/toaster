\name{getNumericColumns}
\alias{getNumericColumns}
\title{Filter numeric columns.}
\usage{
  getNumericColumns(tableInfo, names.only = TRUE,
    include = NULL, except = NULL)
}
\arguments{
  \item{tableInfo}{data frame obtained by calling
  \code{\link{getTableSummary}}.}

  \item{names.only}{logical: if TRUE returns column names
  only, otherwise full rows of \code{tableInfo}.}

  \item{include}{a vector of column names to include.
  Output is restricted to this list.}

  \item{except}{a vector of column names to exclude. Output
  never contains names from this list.}
}
\description{
  Selects numeric columns (names or rows) from table info
  data frame.
}
\seealso{
  \code{\link{getTableSummary}}
}

