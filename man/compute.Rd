\name{compute}
\alias{compute}
\alias{computeAggregates}
\title{Compute aggregate values}
\usage{
  compute(channel, tableName, aggregates = "COUNT(*) cnt",
    by = vector(), where = NULL, stringsAsFactors = FALSE,
    test = FALSE)
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
  Compute aggregates using SQL \code{SELECT...GROUP BY} in
  Aster. Aggregates may be any valid SQL expressions
  (including SQL \code{WINDOW} functions) in context of
  group columns (parameter \code{by}). Neither SQL
  \code{ORDER BY} nor \code{LIMIT} clauses are supported
  (use \code{\link{computeBarchart}} when they are
  required).
}
\examples{
\donttest{
data = compute(channel = conn, tableName = "teams_enh",
               by = c("name || ', ' || park teamname", "lgid", "teamid", "decadeid"),
               aggregates = c("min(name) name", "min(park) park", "avg(rank) rank",
                              "avg(attendance) attendance"))

# compute total strike-outs for each team in decades starting with 1980
# and also percent (share) of team strikeouts within a decade
data = compute(channel = conn, "pitching_enh",
               by = c("teamid", "decadeid"),
               aggregates = c("sum(so) so",
                              "sum(so)/(sum(sum(so)) over (partition by decadeid)) percent"),
               where = "decadeid >= 1980")
}
}

