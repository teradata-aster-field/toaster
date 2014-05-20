\name{computePercentiles}
\alias{computePercentiles}
\title{Compute percentiles of column values.}
\usage{
computePercentiles(channel, tableName, columnName = NULL,
  columnNames = columnName, percentiles = c(0, 5, 10, 25, 50, 75, 90, 95,
  100), by = NULL, where = NULL, nameInDataFrame = "column",
  stringsAsFactors = FALSE, test = FALSE)
}
\arguments{
  \item{channel}{connection object as returned by
  \code{\link{odbcConnect}}}

  \item{tableName}{Aster table name}

  \item{columnName}{deprecated. Use \code{columnNames}
  instead.}

  \item{columnNames}{names of the columns to compute
  percentiles on}

  \item{percentiles}{integer vector with percentiles to
  compute. Values \code{0, 25, 50, 75, 100} will always be
  added if omitted.}

  \item{by}{for optional grouping by one or more values for
  faceting or alike.  Used with \code{\link{createBoxplot}}
  in combination with column name for x-axis and wrap or
  grid faceting.}

  \item{where}{specifies criteria to satisfy by the table
  rows before applying computation. The creteria are
  expressed in the form of SQL predicates (inside
  \code{WHERE} clause).}

  \item{nameInDataFrame}{name of the column in returned
  data frame to store table column name(s) defined by
  parameter \code{columnNames}. \code{NULL} indicates omit
  this column from the data frame (not recommended when
  computing percentiles for two or more columns).}

  \item{stringsAsFactors}{logical: should columns returned
  as character and not excluded by \code{as.is} and not
  converted to anything else be converted to factors?}

  \item{test}{logical: if TRUE show what would be done,
  only (similar to parameter \code{test} in \link{RODBC}
  functions like \link{sqlQuery} and \link{sqlSave}).}
}
\value{
Data frame containing percentile values organized into
following columns: \itemize{ \item \emph{percentile}
percentile to compute (from 0 to 100): will contain all
valid values from \code{percentiles} \item \emph{value}
computed percentile \item \emph{column} table column name.
Override name \code{column} with parameter
\code{nameInDataFrame} or omit this column all together if
\code{NULL}.  \item \emph{by[1], by[2], ...} in presence of
parameter \code{by}, contain values of the grouping columns
for computed percentiles (optional).  }
}
\description{
Compute percentiles including boxplot quartiles across
values of column \code{columnName}. Multiple sets of
percentiles achieved with the parameter \code{by}. Vector
\code{by} may contain arbitrary number of column names: the
percentiles are computed for each combination of values
from these columns. Remember that when using computed
quartiles with function \code{\link{createBoxplot}} it can
utilize up to 3 columns by displaying them along the x-axis
and inside facets.
}
\examples{
\donttest{

# ipouts percentiles for pitching ipouts for AL in 2000s
ipop = computePercentiles(conn, "pitching", "ipouts",
                          where = "lgid = 'AL' and yearid >= 2000")

# ipouts percentiles by league
ipopLg = computePercentiles(conn, "pitching", "ipouts", by="lgid")

}
}

