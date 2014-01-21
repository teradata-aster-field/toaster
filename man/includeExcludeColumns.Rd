\name{includeExcludeColumns}
\alias{includeExcludeColumns}
\title{Helper function to restrict list of columns}
\usage{
  includeExcludeColumns(tableInfo, include, except)
}
\arguments{
  \item{tableInfo}{data frame obtained by calling
  \code{\link{getTableSummary}}.}

  \item{include}{a vector of column names to include.
  Output is restricted to this list.}

  \item{except}{a vector of column names to exclude. Output
  never contains names from this list.}
}
\description{
  Helper function to restrict list of columns
}

