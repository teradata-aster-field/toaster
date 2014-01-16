\name{computeCorrelations}
\alias{computeCorrelations}
\title{Compute correlations between all or specified numeric columns in a table}
\usage{
  computeCorrelations(channel, tableName, tableInfo,
    include, except = NULL, where = NULL, test = FALSE)
}
\arguments{
  \item{channel}{connection object as returned by
  \code{\link{odbcConnect}}}

  \item{tableName}{database table name}

  \item{tableInfo}{pre-built summary of data to use (must
  have with \code{test=TRUE})}

  \item{include}{a vector of column names to include.
  Output never contains attributes other than in the list.}

  \item{except}{a vector of column names to exclude. Output
  never contains attributes from the list.}

  \item{where}{SQL WHERE clause limiting data from the
  table (use SQL as if in WHERE clause but omit keyword
  WHERE)}

  \item{test}{logical: if TRUE show what would be done,
  only (similar to parameter \code{test} in \link{RODBC}
  functions like \link{sqlQuery} and \link{sqlSave}).}
}
\description{
  Compute correlations between all or specified numeric
  columns in a table
}

