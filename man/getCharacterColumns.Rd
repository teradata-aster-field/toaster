\name{getCharacterColumns}
\alias{getCharacterColumns}
\title{Filter character columns.}
\usage{
getCharacterColumns(tableInfo, names.only = TRUE, include = NULL,
  except = NULL)
}
\arguments{
  \item{tableInfo}{data frame obtained by calling
  \code{\link{getTableSummary}}.}

  \item{include}{a vector of column names to include.
  Output is restricted to this list.}

  \item{except}{a vector of column names to exclude. Output
  never contains names from this list.}

  \item{names.only}{logical: if TRUE returns column names
  only, otherwise full rows of \code{tableInfo}.}
}
\description{
Selects character columns (names or rows) from table info
data frame.
}
\examples{
\donttest{
pitchingInfo = getTableSummary(channel=conn, 'pitching_enh')
getCharacterColumns(pitchingInfo)
char_cols_df = getCharacterColumns(pitchingInfo, names.only=FALSE)
}
}
\seealso{
\code{\link{getTableSummary}}
}

