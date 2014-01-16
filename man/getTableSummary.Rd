\name{getTableSummary}
\alias{getTableSummary}
\title{Augment Column table with column statistics computed in Aster}
\usage{
  getTableSummary(channel, tableName, include = NULL,
    except = NULL, modeValue = FALSE,
    percentiles = c(0, 5, 10, 25, 50, 75, 90, 95, 100),
    where = NULL, mock = FALSE)
}
\arguments{
  \item{channel}{object as returned by
  \code{\link{odbcConnect}}.}

  \item{tableName}{table name.}

  \item{include}{a vector of column names to include.
  Output never contains attributes other than in the list.}

  \item{except}{a vector of column names to exclude. Output
  never contains attributes from the list.}

  \item{modeValue}{logical indicates if mode values should
  be computed.}

  \item{percentiles}{list of percentiles (integers between
  0 and 100) to collect (always collects 25th and 75th for
  IQR calculation). There is no penalty in specifying more
  percentiles as they get calculated in a single call for
  each column - no matter how many different values are
  requested.}

  \item{where}{SQL WHERE clause limiting data from the
  table (use SQL as if in WHERE clause but omit keyword
  WHERE).}

  \item{mock}{logical: if TRUE returns pre-computed table
  statistics for tables \code{pitching} or \code{batting},
  only.}
}
\description{
  Computes columns statistics for all or specified table
  columns and adds them to the data frame with basic ODBC
  table metadata obtained with \code{\link{sqlColumns}}.
}
\seealso{
  \link{sqlColumns}
}

