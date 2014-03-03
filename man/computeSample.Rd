\name{computeSample}
\alias{computeSample}
\title{Randomly sample data from the table}
\usage{
  computeSample(channel, tableName, sampleFraction,
    sampleSize, include = NULL, except = NULL,
    where = NULL, stringsAsFactors = FALSE, test = FALSE)
}
\arguments{
  \item{sampleFraction}{one or more sample fractions to use
  in the sampling of data.}

  \item{sampleSize}{total sample size (applies only when
  \code{sampleFraction} is missing).}
}
\description{
  Randomly sample data from the table
}

