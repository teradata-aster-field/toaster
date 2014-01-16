\name{joinTables}
\alias{joinTables}
\title{Joins two tables and lets use result in place of a table name.}
\usage{
  joinTables(table1, table2, join = "inner", alias1 = "a",
    alias2 = "b", joinColumns, joinCondition = NULL,
    select1 = "*", select2 = NULL, alias = "t",
    smart = FALSE, channel = NULL)
}
\arguments{
  \item{table1}{name of 1st table to join (left position)}

  \item{table2}{name of 2d table to join (right position)}

  \item{join}{type of join:
  \code{c('inner','left','right','full')}}

  \item{alias1}{alias for 1st table}

  \item{alias2}{alias for 2d table}

  \item{joinColumns}{one or more columns to join tables on.
  These column names are to be found in both joined tables.
  If tables have different column names to join on or
  condition is not simple equality then use parameter
  \code{joinCondition}}

  \item{joinCondition}{explicitly defined join condition,
  \code{joinColumns} will be ignored.}

  \item{select1}{list of columns to select from 1st table}

  \item{select2}{list of columns to select from 2d table}

  \item{alias}{table alias for join resultto use for
  resulting join}

  \item{smart}{if TRUE then select list for join will be
  constructed by function to eliminate duplicate columns
  (default is FALSE)}

  \item{channel}{if \code{smart} is TRUE then required to
  access table info}
}
\description{
  Joins two tables and lets use result in place of a table
  name.
}
\examples{
\donttest{
# 2-column inner join
joinTable('pitching','teams', c('teamid','yearid'))
# same with columns selected from both tables
joinTab joinTables('pitching', 'teams', 'inner', 'a', 'b', c('teamid','yearid'),
                   select2=c('rank','g','w','l','divwin','wcwin','lgwin','wswin'),
                   alias='myalis')
}
}

