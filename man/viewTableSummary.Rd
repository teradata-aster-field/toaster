\name{viewTableSummary}
\alias{viewTableSummary}
\title{Invoke a Data Viewer}
\usage{
  viewTableSummary(tableInfo, include = NULL,
    except = NULL, basic = FALSE, percentiles = FALSE)
}
\arguments{
  \item{tableInfo}{data frame with columns statistics to
  display.}

  \item{include}{a vector of column names to include.
  Output never contains attributes other than in the list.}

  \item{except}{a vector of column names to exclude. Output
  never contains attributes from the list.}

  \item{basic}{logical: if TRUE display minimum, maximum,
  average, deviation and mode (if present)}

  \item{percentiles}{logical: if TRUE display percentiles}
}
\description{
  view computed column statistics in a spreadsheet-style
  viewer in R.
}
\details{
  When both parameters \code{basic} and \code{percentiles}
  are FALSE view displays \emph{all} statistics.
}
\seealso{
  \code{\link{getTableSummary}}
}

