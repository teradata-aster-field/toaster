\name{getMatchingColumns}
\alias{getMatchingColumns}
\title{Filter columns by pattern.}
\usage{
getMatchingColumns(pattern, channel, tableName, tableInfo, names.only = TRUE,
  ignore.case = TRUE, invert = FALSE)
}
\arguments{
  \item{pattern}{character string containing a
  \link{regular expression} to be matched in the given
  table info.}

  \item{channel}{connection object as returned by
  \code{\link{odbcConnect}}. Only used in combination with
  \code{tableName}.}

  \item{tableName}{Aster table name to use. If missing then
  \code{tableInfo} will be used instead.}

  \item{tableInfo}{data frame obtained by calling
  \code{\link{getTableSummary}} or
  \code{\link{sqlColumns}}.}

  \item{names.only}{logical: if TRUE returns column names
  only, otherwise full rows of \code{tableInfo}.}

  \item{ignore.case}{if TRUE case is ignored during
  matching, otherwise matching is case sensitive.}

  \item{invert}{logical. if TRUE return columns that do not
  match.}
}
\description{
Selects columns with names matching regular expression
pattern.
}
\seealso{
\code{\link{grep}}
}

