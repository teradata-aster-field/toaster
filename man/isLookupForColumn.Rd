\name{isLookupForColumn}
\alias{isLookupForColumn}
\title{Validation of lookup data in database}
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
\value{
  TRUE if lookup relationship between tables and columns
  holds, otherwise FALSE
}
\description{
  Convinience function to test if table has indeed a lookup
  column in another table.
}

