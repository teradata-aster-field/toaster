\name{computeTf}
\alias{computeTf}
\title{Compute term frequencies on a corpus.}
\usage{
computeTf(channel, tableName, docId, textColumns, parser,
  weighting = "normal", where = NULL, idSep = "-", idNull = "(null)",
  test = FALSE)
}
\arguments{
  \item{channel}{connection object as returned by
  \code{\link{odbcConnect}}}

  \item{tableName}{Aster table name}

  \item{docId}{vector with one or more column names
  comprising unique document id.  Values are concatenated
  with \code{idSep}. Database NULLs are replaced with
  \code{idNull} string.}

  \item{textColumns}{one or more names of columns with
  text. Multiple coumn are concatenated into single text
  field first.}

  \item{parser}{type of parser to use on text. For example,
  \code{ngram(2)} parser generates 2-grams (ngrams of
  length 2), \code{token(2)} parser generates 2-word
  combinations of terms within documents.}

  \item{weighting}{term frequency formula to compute the tf
  value.}

  \item{idSep}{separator when concatenating 2 or more
  document id columns (see \code{docId}).}

  \item{idNull}{string to replace NULL value in document id
  columns.}

  \item{where}{specifies criteria to satisfy by the table
  rows before applying computation. The criteria are
  expressed in the form of SQL predicates (inside
  \code{WHERE} clause).}

  \item{test}{logical: if TRUE show what would be done,
  only (similar to parameter \code{test} in \link{RODBC}
  functions \link{sqlQuery} and \link{sqlSave}).}
}
\description{
Compute term frequencies on a corpus.
}
\seealso{
\code{\link{nGram}}, \code{\link{token}}
}

