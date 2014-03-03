\name{createPopPyramid}
\alias{createPopPyramid}
\title{Create plot with Population Pyramid visualization}
\usage{
  createPopPyramid(data, bin = "bin_start",
    count = "bin_count", divideBy, values = NULL,
    fillColours = c("blue", "red"), mainColour = "black",
    facet = NULL, ncol = 1, facetScales = "fixed",
    baseSize = 12, baseFamily = "sans",
    title = paste("Population Pyramid Histogram"),
    xlab = bin, ylab = count, legendPosition = "right",
    defaultTheme = theme_bw(base_size = baseSize),
    themeExtra = NULL)
}
\arguments{
  \item{data}{data frame contains 2 histograms for the same
  bins. Bins are divided into 2 sets with parameter
  \code{divideBy}.}

  \item{bin}{name of a column containing bin labels or
  interval values}

  \item{count}{name of a column containing bin values or
  counts (bin size)}

  \item{divideBy}{name of the column to divide data into
  two histograms}

  \item{values}{two-valued vector containing values in
  \code{divideBy} (optional). If missing then it uses 1st 2
  values from column \code{divideBy} (sorted with default
  order).}

  \item{fillColours}{2-value vector with colours for left
  and right histograms.}

  \item{mainColour}{histogram bar colour.}

  \item{facet}{name of a column to divide plot into facets
  for specificed parameter (defualt is NULL - no facets).
  If facet is single value then facet wrap applied (see
  \code{\link{facet_wrap}}), otherwise facet grid (see
  \code{\link{facet_grid}} with 1st 2 values of the
  vector.}

  \item{ncol}{number of facet columns (applies when single
  facet column supplied only - see parameter
  \code{facet}).}

  \item{facetScales}{Are scales shared across all facets:
  "fixed" - all are the same, "free_x" - vary across rows
  (x axis), "free_y" - vary across columns (Y axis)
  (default), "free" - both rows and columns (see in
  \code{facet_wrap} parameter \code{scales} )}

  \item{baseSize}{base font size}

  \item{baseFamily}{base font family}

  \item{title}{plot title.}

  \item{xlab}{a label for the x axis, defaults to a
  description of x.}

  \item{ylab}{a label for the y axis, defaults to a
  description of y.}

  \item{legendPosition}{the position of legends. ("left",
  "right", "bottom", "top", or two-element numeric vector).
  "none" is no legend.}

  \item{defaultTheme}{plot theme to use, default is
  \code{theme_bw}}

  \item{themeExtra}{any additional \code{ggplot2} theme
  attributes to add}
}
\description{
  Create population pyramid type of visual: two
  back-to-back bar graphs on the same category class (e.g.
  age) placed on Y-axis and distribution (population)
  placed on the X-axis. Bar graphs correspond to two
  distinct groups, e.g. sex (male and female), baseball
  leagues (AL and NL), or customer types (new customers and
  established customers).
}
\examples{
\donttest{
pitchingInfo = getTableSummary(asterConn, tableName='pitching', where='yearid between 2000 and 2013')
battingInfo = getTableSummary(asterConn, tableName='batting', where='yearid between 2000 and 2013')

salaryHistAll = computeHistogram(asterConn, tableName='public.salaries', columnName='salary',
                                binsize=200000, startvalue=0,
                                by='lgid', where='yearID between 2000 and 2013')
createPopPyramid(data=salaryHistAll, bin='bin_start', count='bin_count', divideBy='lgid', values=c('NL','AL'),
                 title="Salary Pyramid by MLB Leagues", xlab='Salary', ylab='Player Count')

salaryHist5Mil = computeHistogram(asterConn, tableName='salaries', columnName='salary',
                                  binsize=100000, startvalue=0, endvalue=5000000,
                                  by='lgid', where='yearID between 2000 and 2013')
createPopPyramid(data=salaryHist5Mil, divideBy='lgid', values=c('NL','AL'),
                 title="Salary Pyramid by MLB Leagues (less 5M only)", xlab='Salary', ylab='Player Count')

eraHist = computeHistogram(asterConn, tableName='pitching', columnName='era', binsize=.1, startvalue=0, endvalue=10,
                           by='lgid', where='yearid between 2000 and 2013')
createPopPyramid(data=eraHist, divideBy='lgid', values=c('NL','AL'),
                 title="ERA Pyramid by MLB Leagues", xlab='ERA', ylab='Player Count')

# Log ERA
eraLogHist = computeHistogram(asterConn, tableName='pitching', columnName='era_log', binsize=.02, startvalue=-0.42021640338318984325,
                              endvalue=2.2764618041732441,
                              by='lgid', where='yearid between 2000 and 2013 and era > 0')
createPopPyramid(data=eraLogHist, divideBy='lgid', values=c('NL','AL'),
                 title="log(ERA) Pyramid by MLB Leagues", xlab='log(ERA)', ylab='Player Count')

# Batting (BA)
battingHist = computeHistogram(asterConn, tableName='batting_enh', columnName='ba', binsize=.01, startvalue=0.01, endvalue=0.51,
                               by='lgid', where='yearid between 2000 and 2013')
createPopPyramid(data=battingHist, divideBy='lgid', values=c('NL','AL'),
                 title="Batting BA Pyramid by MLB Leages", xlab='BA', ylab='Player Count')
}
}

