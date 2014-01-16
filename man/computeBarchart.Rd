\name{computeBarchart}
\alias{computeBarchart}
\title{Compute Barchart for a category and arbitrary number of aggregates.}
\usage{
  computeBarchart(channel, tableName, category,
    aggregates = "COUNT(*) cnt", percent = FALSE,
    where = NULL, orderBy = NULL, top = NULL, by = NULL,
    withMelt = FALSE, stringsAsFactors = FALSE,
    test = FALSE)
}
\arguments{
  \item{channel}{object as returned by
  \code{\link{odbcConnect}}}

  \item{tableName}{table name}

  \item{category}{column name or expression corresponding
  to bar chart category. Name may be valid SQL expression
  and can contain otional alias (e.g.
  \code{"UPPER(car_make) make"})}

  \item{aggregates}{SQL aggregates to compute. Aggregates
  may have optional aliases like in \code{"AVG(era)
  avg_era"}}

  \item{by}{for optional grouping by one or more columns
  for faceting or alike (effectively these elements will be
  part of \code{GROUP BY ...})}

  \item{orderBy}{list of column names, aliases, references
  or their combinations to use in SQL \code{ORDER BY}
  clause}

  \item{top}{if specified indicates number of bars to
  include in bar plot. In combination with \code{orderBy}
  it works as computing first \code{top} results.}

  \item{withMelt}{logical if TRUE then uses \pkg{reshape2}
  \code{\link{melt}} to transform result data frame
  aggregate values into a molten data frame}

  \item{stringsAsFactors}{logical: should columns returned
  as character and not excluded by as. is and not converted
  to anything else be converted to factors?}

  \item{test}{logical: if TRUE show what would be done,
  only (similar to parameter \code{test} in \link{RODBC}
  functions like \link{sqlQuery} and \link{sqlSave}).}
}
\description{
  Computes data suitable for bar chart plotting with single
  category corresponding to a column or an expression.
  Function can compute arbitrary number of aggregates.
}
\examples{
\donttest{
# Compute average team season era, walks, and hits for each decaded starting with 1980
computeBarchart(asterConn, "teams_enh", "teamid team", c("avg(era) era", "avg(bb) bb", "avg(h) h"),
                where="yearid >=1980", by=c("decadeid"))
}
}
\seealso{
  computeHistogram
}

