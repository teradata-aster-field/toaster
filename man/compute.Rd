\name{compute}
\alias{compute}
\alias{computeAggregates}
\title{Compute table aggregates.}
\usage{
  compute(channel, tableName,
    aggregates = c("COUNT(*) cnt"), by = vector(),
    where = NULL, stringsAsFactors = FALSE, test = FALSE)
}
\arguments{
  \item{channel}{connection object as returned by
  \code{\link{odbcConnect}}}

  \item{tableName}{table name}

  \item{by}{column names and/or expressions to use as
  aggregates (with SQL \code{GROUP BY ...}). Each can be
  column name or valid SQL expression with otional alias
  (e.g. \code{"UPPER(car_make) make"})}

  \item{aggregates}{SQL aggregates to compute. Aggregates
  may have optional aliases like in \code{"AVG(era)
  avg_era"}}

  \item{where}{SQL WHERE clause limiting data from the
  table (use SQL as if in WHERE clause but omit keyword
  WHERE)}

  \item{stringsAsFactors}{logical: should character vectors
  returned as part of results be converted to factors?}

  \item{test}{logical: if TRUE show what would be done,
  only (similar to parameter \code{test} in \link{RODBC}
  functions like \link{sqlQuery} and \link{sqlSave}).}
}
\description{
  Computes aggregates by means of SQL \code{SELECT ...
  GROUP BY} on Aster table.
}
\examples{
\donttest{
data = compute(channel = conn, tableName = "teams_enh",
               by = c("name || ', ' || park teamname", "lgid", "teamid", "decadeid"),
               aggregates = c("min(name) name", "min(park) park", "avg(rank) rank",
                              "avg(attendance) attendance")
               )
}
}

