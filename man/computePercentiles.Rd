\name{computePercentiles}
\alias{computePercentiles}
\title{Compute percentiles}
\usage{
  computePercentiles(channel, tableName, columnName,
    percentiles = c(0, 5, 10, 25, 50, 75, 90, 95, 100),
    by = NULL, where = NULL, stringsAsFactors = FALSE,
    test = FALSE)
}
\arguments{
  \item{channel}{connection object as returned by
  \code{\link{odbcConnect}}}

  \item{tableName}{Aster table name}

  \item{columnName}{name of the column to compute
  percentiles on}

  \item{by}{for optional grouping by one or more values for
  faceting or alike.  If used with
  \code{\link{createBoxplot}} then use first name for
  x-axis and the rest for wrap or grid faceting.}

  \item{where}{specifies criteria to satisfy by the table
  rows before applying computation. The creteria are
  expressed in the form of SQL predicates (inside
  \code{WHERE} clause).}

  \item{stringsAsFactors}{logical: should columns returned
  as character and not excluded by as. is and not converted
  to anything else be converted to factors?}

  \item{test}{logical: if TRUE show what would be done,
  only (similar to parameter \code{test} in \link{RODBC}
  functions like \link{sqlQuery} and \link{sqlSave}).}
}
\description{
  Compute percentiles including boxplot quartiles.
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

