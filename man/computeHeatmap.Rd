\name{computeHeatmap}
\alias{computeHeatmap}
\title{computes data frame for heatmap visualizations}
\usage{
  computeHeatmap(channel, tableName, dimension1,
    dimension2, aggregateFun = "COUNT(*)",
    aggregateAlias = "cnt", percent = FALSE,
    withMelt = FALSE, id.vars, measure.vars,
    variable.name = "variable", value.name = "value",
    melt.na.rm = FALSE, where = NULL, by = NULL,
    test = FALSE)
}
\arguments{
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
  only (similar to parameter \code{test} in \link{RODBC}
  functions like \link{sqlQuery} and \link{sqlSave}).}
}
\description{
  computes data frame for heatmap visualizations
}

