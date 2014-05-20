\name{computeTfIdf}
\alias{computeTfIdf}
\title{Compute Term Frequency - Inverse Document Frequency on a corpus.}
\usage{
computeTfIdf(channel, tableName, docId, textColumns, parser, where = NULL,
  idSep = "-", idNull = "(null)", adjustDocumentCount = FALSE,
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

  \item{idSep}{separator when concatenating 2 or more
  document id columns (see \code{docId}).}

  \item{idNull}{string to replace NULL value in document id
  columns.}

  \item{adjustDocumentCount}{logical: if TRUE then number
  of documents 2 will be increased by 1.}

  \item{where}{specifies criteria to satisfy by the table
  rows before applying computation. The criteria are
  expressed in the form of SQL predicates (inside
  \code{WHERE} clause).}

  \item{test}{logical: if TRUE show what would be done,
  only (similar to parameter \code{test} in \link{RODBC}
  functions \link{sqlQuery} and \link{sqlSave}).}
}
\description{
Compute Term Frequency - Inverse Document Frequency on a
corpus.
}
\seealso{
\code{\link{nGram}}, \code{\link{token}}
}

