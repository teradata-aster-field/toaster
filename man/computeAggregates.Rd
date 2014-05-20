\name{computeAggregates}
\alias{computeAggregates}
\title{Compute aggregate values.}
\usage{
computeAggregates(channel, tableName, aggregates = c("COUNT(*) cnt"),
  by = vector(), where = NULL, stringsAsFactors = FALSE, test = FALSE)
}
\arguments{
  \item{channel}{connection object as returned by
  \code{\link{odbcConnect}}}

  \item{tableName}{table name}

  \item{by}{character vecotr of column names and/or
  expressions on which grouping is performed (with SQL
  \code{GROUP BY ...}). Each can be a column or a valid SQL
  non-aggregate expression with otional alias separated by
  space (e.g. \code{"UPPER(car_make) make"}).}

  \item{aggregates}{vector of SQL aggregates to compute.
  Aggregates may have optional aliases like in
  \code{"AVG(era) avg_era"}}

  \item{where}{specifies criteria to satisfy by the table
  rows before applying computation. The creteria are
  expressed in the form of SQL predicates (inside
  \code{WHERE} clause).}

  \item{stringsAsFactors}{logical: should character vectors
  returned as part of results be converted to factors?}

  \item{test}{logical: if TRUE show what would be done,
  only (similar to parameter \code{test} in \link{RODBC}
  functions like \link{sqlQuery} and \link{sqlSave}).}
}
\description{
Compute aggregates using SQL \code{SELECT...GROUP BY} in
Aster. Aggregates may be any valid SQL expressions
(including SQL \code{WINDOW} functions) in context of group
columns (parameter \code{by}). Neither SQL \code{ORDER BY}
nor \code{LIMIT} clauses are supported (use
\code{\link{computeBarchart}} when they are required).
}
\examples{
\donttest{
data = computeAggregates(channel = conn, tableName = "teams_enh",
               by = c("name || ', ' || park teamname", "lgid", "teamid", "decadeid"),
               aggregates = c("min(name) name", "min(park) park", "avg(rank) rank",
                              "avg(attendance) attendance"))

# compute total strike-outs for each team in decades starting with 1980
# and also percent (share) of team strikeouts within a decade
data = computeAggregates(channel = conn, "pitching_enh",
               by = c("teamid", "decadeid"),
               aggregates = c("sum(so) so",
                              "sum(so)/(sum(sum(so)) over (partition by decadeid)) percent"),
               where = "decadeid >= 1980")
}
}

