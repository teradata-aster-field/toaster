\name{computeSample}
\alias{computeSample}
\title{Randomly sample data from the table}
\usage{
  computeSample(channel, tableName, sampleFraction,
    sampleSize, include = NULL, except = NULL,
    where = NULL, stringsAsFactors = FALSE, test = FALSE)
}
\arguments{
  \item{channel}{connection object as returned by
  \code{\link{odbcConnect}}}

  \item{tableName}{table name}

  \item{sampleFraction}{one or more sample fractions to use
  in the sampling of data. (multipe sampling fractions are
  not yet supported.)}

  \item{sampleSize}{total sample size (applies only when
  \code{sampleFraction} is missing).}

  \item{include}{a vector of column names to include.
  Output never contains attributes other than in the list.}

  \item{except}{a vector of column names to exclude. Output
  never contains attributes from the list.}

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
  Draws a sample of rows from the table randomly. The
  function offers two sampling schemes: - a simple binomial
  (Bernoulli) sampling on a row-by-row basis with given
  sample rate(s) - sampling a given number of rows without
  replacement The sampling can be applied to the entire
  table or can be refined with conditions.
}
\examples{
\donttest{
batters = computeSample(conn, "batting", sampleFraction=0.01)
dim(batters)

pitchersAL = computeSample(conn, "pitching", sampleSize=1000,
                           where="lgid = 'AL'")
dim(ptichersAL)
}
}

