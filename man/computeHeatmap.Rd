\name{computeHeatmap}
\alias{computeHeatmap}
\title{computes data frame for heatmap visualizations}
\usage{
  computeHeatmap(channel, tableName, dimension1,
    dimension2, aggregateFun = "COUNT(*)",
    aggregateAlias = "cnt", withMelt = FALSE, id.vars,
    measure.vars, variable.name = "variable",
    value.name = "value", melt.na.rm = FALSE, where = NULL,
    by = NULL, test = FALSE)
}
\arguments{
  \item{channel}{connection object as returned by
  \code{\link{odbcConnect}}}

  \item{tableName}{table name}

  \item{dimension1}{name of the column for for heatmap x
  values. This value along with \code{dimension2} are x and
  y scales of heatmap table.}

  \item{dimension2}{name of the column for for heatmap y
  values. This value along with \code{dimension1} are x and
  y scales of heatmap table.}

  \item{aggregateFun}{vector with aggregate functions to
  compute and use for for heatmap values. In SQL it
  translates to \code{GROUP BY dimension1, dimension2} and
  computes one or more aggregates.}

  \item{aggregateAlias}{vector of aliases for aggregate
  values in SQL. This becomes names of value columns in
  heatmap data frame. Subsequently, \code{createHeatmap}
  will use these values to assign color (fill), text, and
  threshold values to heatmap cells.}

  \item{withMelt}{logical if TRUE then uses \pkg{reshape2}
  \code{\link{melt}} to transform result data frame
  aggregate values into a molten data frame}

  \item{id.vars}{see \code{melt.data.frame}}

  \item{measure.vars}{see \code{melt.data.frame}}

  \item{variable.name}{see \code{melt.data.frame}}

  \item{value.name}{see \code{melt.data.frame}}

  \item{melt.na.rm}{see \code{melt.data.frame}}

  \item{where}{SQL WHERE clause limiting data from the
  table (use SQL as if in WHERE clause but omit keyword
  WHERE)}

  \item{by}{for optional grouping by one or more values for
  faceting or alike}

  \item{test}{logical: if TRUE show what would be done,
  only (similar to parameter \code{test} in \pkg{RODBC}
  functions: \link{sqlQuery} and \link{sqlSave}).}
}
\description{
  computes data frame for heatmap visualizations
}
\examples{
\donttest{
hm = computeHeatmap(asterConn, "teams_enh", 'franchid', 'decadeid', 'avg(w)', 'w', where="decadeid >= 1950")
hm$decadeid = factor(hm$decadeid)
createHeatmap(hm, 'decadeid', 'franchid', 'w')

# with diverging color gradient
hm = computeHeatmap(asterConn, "teams_enh", 'franchid', 'decadeid', 'avg(w-l)', 'wl', where="decadeid >= 1950")
hm$decadeid = factor(hm$decadeid)
createHeatmap(hm, 'decadeid', 'franchid', 'wl', divergingColourGradient = TRUE)
}
}
\seealso{
  createHeatmap
}

