\name{computeHistogram}
\alias{computeHistogram}
\title{Compute table column histogram}
\usage{
  computeHistogram(channel, tableName, columnName,
    tableInfo = NULL, columnFrequency = FALSE,
    binMethod = "manual", binsize = NULL,
    startvalue = NULL, endvalue = NULL, numbins = NULL,
    useIQR = TRUE, datepart = NULL, where = NULL,
    by = NULL, test = FALSE, oldStyle = FALSE)
}
\arguments{
  \item{channel}{connection object as returned by
  \code{\link{odbcConnect}}}

  \item{tableName}{Aster table name}

  \item{columnName}{table column name to compute histogram}

  \item{tableInfo}{pre-built summary of data to use
  (require when \code{test=TRUE}). See
  \code{\link{getTableSummary}}.}

  \item{columnFrequency}{logical indicates to build
  histogram of frequencies of column}

  \item{binMethod}{one of several methods to determine
  number and size of bins: \code{'manual'} indicates to use
  paramters below, both \code{'Sturges'} or \code{'Scott'}
  will use corresponding methods of computing number of
  bins and width (see
  \url{http://en.wikipedia.org/wiki/Histogram#Number_of_bins_and_width}).}

  \item{binsize}{size (width) of discrete intervals
  defining histogram (all bins are equal)}

  \item{startvalue}{lower end (bound) of values to include
  in histogram}

  \item{endvalue}{upper end (bound) of values to include in
  histogram}

  \item{numbins}{number of bins to use in histogram}

  \item{useIQR}{logical indicates use of IQR interval to
  compute cutoff lower and upper bounds for values to be
  included in histogram: \code{[Q1 - 1.5 * IQR, Q3 + 1.5 *
  IQR], IQR = Q3 - Q1}}

  \item{datepart}{field to extract from timestamp/date/time
  column to build histogram on}

  \item{where}{SQL WHERE clause limiting data from the
  table (use SQL as if in WHERE clause but omit keyword
  WHERE)}

  \item{by}{for optional grouping by one or more values for
  faceting or alike}

  \item{test}{logical: if TRUE show what would be done,
  only (similar to parameter \code{test} in \link{RODBC}
  functions like \link{sqlQuery} and \link{sqlSave}).}

  \item{oldStyle}{logical indicates if old style histogram
  paramters are in use (before Aster AF 5.11)}
}
\description{
  \code{computeHistogram} uses Aster SQL/MR histogram
  function to compute a histogram on given table column.
}
\examples{
\donttest{
# Histogram of team ERA distribution: Rangers vs. Yankees in 2000s
h2000s = computeHistogram(channel=conn, tableName='pitching_enh', columnName='era',
                          binsize=0.2, startvalue=0, endvalue=10, by='teamid',
                          where="yearID between 2000 and 2012 and teamid in ('NYA','TEX')")
createHistogram(h2000s, fill='teamid', facet='teamid',
                title='TEX vs. NYY 2000-2012', xlab='ERA', ylab='count',
                legendPosition='none')
}
}
\seealso{
  computeBarchart
}

