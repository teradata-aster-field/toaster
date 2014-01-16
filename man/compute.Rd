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
  \item{channel}{object as returned by
  \code{\link{odbcConnect}}}

  \item{tableName}{table name}

  \item{by}{column names and expressions to group
  aggregates (with SQL \code{GROUP BY ...}). It may be
  table column names, valid SQL expressions and can contain
  otional alias (e.g. \code{"UPPER(car_make) make"})}

  \item{aggregates}{SQL aggregates to compute. Aggregates
  may have optional aliases like in \code{"AVG(era)
  avg_era"}}

  \item{stringsAsFactors}{logical: should columns returned
  as character and not excluded by as. is and not converted
  to anything else be converted to factors?}

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
data = compute(asterConn, "teams_enh",
               by = c("name || ', ' || park teamname", "lgid", "teamid", "decadeid"),
               aggregates = c("min(name) name", "min(park) park", "avg(rank) rank", "avg(attendance) attendance")
               )
}
}

