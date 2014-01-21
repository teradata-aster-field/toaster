\name{isLookupForColumn}
\alias{isLookupForColumn}
\title{convinience function to test if table has indeed a lookup column in another table.}
\usage{
  isLookupForColumn(channel, tableName, columnName,
    lookupTable, lookupColumn = columnName,
    ignoreCase = FALSE)
}
\arguments{
  \item{channel}{object as returned by
  \code{\link{odbcConnect}}.}

  \item{tableName}{data table name.}

  \item{columnName}{column name in data table.}

  \item{lookupTable}{lookup table name.}

  \item{lookupColumn}{column name in lookup table.}

  \item{ignoreCase}{ignore case when comparing data. May
  produce slower results, especially on partitioned data.}
}
\description{
  convinience function to test if table has indeed a lookup
  column in another table.
}

