#' List Aster numeric data types.
#' 
#' @return character vector with names of Aster numeric data types
#' @export
#' @examples
#' getNumericTypes()
#' 
getNumericTypes <- function () {
  return( c('integer',
            'numeric',
            'bigint',
            'smallint',
            'real',
            'double precision',
            'serial',
            'bigserial',
            'float',
            'decimal')
  )
}

#' List Aster character data types.
#'
#' @return character vector with names of Aster character data types
#' @export
#' @examples 
#' getCharacterTypes()
#' 
#' 
getCharacterTypes <- function() {
  return(c('varchar',
           'char',
           'character')
  )
}

#' List Aster temporal data types.
#' 
#' @return character vector with names of Aster temporal data types
#' @export
#' @examples
#' getTemporalTypes()
#' 
getTemporalTypes <- function() {
  return(c('date', 
           'timestamp without time zone', 
           'timestamp with time zone',
           'time without time zone',
           'time with time zone')
  )
}

getTypes <- function(types) {
  
  result = character(0)
  
  if ('numeric' %in% types) {
    result = union(result, getNumericTypes())
  }
  
  if ('character' %in% types) {
    result = union(result, getCharacterTypes())
  }
  
  if ('temporal' %in% types) {
    result = union(result, getTemporalTypes())
  }
  
  return(result)
}

#' Filter numeric columns.
#'
#' Select numeric columns (names or rows) from table info data frame.
#' 
#' @param tableInfo data frame obtained by calling \code{\link{getTableSummary}}.
#' @param names.only logical: if TRUE returns column names only, otherwise full rows of \code{tableInfo}.
#' @param include a vector of column names to include. Output is restricted to this list.
#' @param except a vector of column names to exclude. Output never contains names from this list.
#' 
#' @seealso \code{\link{getTableSummary}}
#' @export
#' @examples
#' \donttest{
#' pitchingInfo = getTableSummary(channel=conn, 'pitching_enh')
#' getNumericColumns(pitchingInfo)
#' num_cols_df = getNumericColumns(pitchingInfo, names.only=FALSE)
#' }
getNumericColumns <- function (tableInfo, names.only=TRUE, include=NULL, except=NULL) {
  
  numeric_types = getNumericTypes()
  
  return(getColumns(tableInfo, numeric_types, names.only, include, except))
}



#' Filter character columns.
#' 
#' Selects character columns (names or rows) from table info data frame.
#' 
#' @param tableInfo data frame obtained by calling \code{\link{getTableSummary}}.
#' @param include a vector of column names to include. Output is restricted to this list.
#' @param except a vector of column names to exclude. Output never contains names from this list.
#' @param names.only logical: if TRUE returns column names only, otherwise full rows of \code{tableInfo}.
#' @seealso \code{\link{getTableSummary}}
#' @export
#' @examples
#' \donttest{
#' pitchingInfo = getTableSummary(channel=conn, 'pitching_enh')
#' getCharacterColumns(pitchingInfo)
#' char_cols_df = getCharacterColumns(pitchingInfo, names.only=FALSE)
#' }
getCharacterColumns <- function (tableInfo, names.only=TRUE, include=NULL, except=NULL) {
  
  char_types = getCharacterTypes()
  
  return(getColumns(tableInfo, char_types, names.only, include, except))
}


#' Filter Date and Time Table Columns.
#' 
#' Selects date and time columns (names or rows) from table info data frame.
#' 
#' @param tableInfo data frame obtained by calling \code{\link{getTableSummary}}.
#' @param include a vector of column names to include. Output is restricted to this list.
#' @param except a vector of column names to exclude. Output never contains names from this list.
#' @param names.only logical: if TRUE returns column names only, otherwise full rows of \code{tableInfo}.
#' @seealso \code{\link{getTableSummary}}
#' @export
#' @examples
#' \donttest{
#' masterInfo = getTableSummary(channel=conn, 'master')
#' getDateTimeColumns(masterInfo)
#' date_cols_df = getDateTimeColumns(masterInfo, names.only=FALSE)
#' }
getDateTimeColumns <- function (tableInfo, names.only=TRUE, include=NULL, except=NULL) {
  datetime_types = getTemporalTypes()
  
  return(getColumns(tableInfo, datetime_types, names.only, include, except))
}


#' Filter columns by pattern.
#' 
#' Selects columns with names matching regular expression pattern.
#' 
#' @param pattern character string containing a \link{regular expression} to be matched in the given table info.
#' @param channel connection object as returned by \code{\link{odbcConnect}}. Only used in combination with \code{tableName}.
#' @param tableName Aster table name to use. If missing then \code{tableInfo} will be used instead.
#' @param tableInfo data frame obtained by calling \code{\link{getTableSummary}} or \code{\link{sqlColumns}}.
#' @param names.only logical: if TRUE returns column names only, otherwise full rows of \code{tableInfo}.
#' @param ignore.case if TRUE case is ignored during matching, otherwise matching is case sensitive.
#' @param invert logical. if TRUE return columns that do not match.
#' @seealso \code{\link{grep}}
#' @export
#' 
getMatchingColumns <- function (pattern, channel, tableName, tableInfo, names.only = TRUE, 
                                ignore.case = TRUE, invert = FALSE) {
  
  if (!missing(tableName)) {
    tableInfo = sqlColumns(channel, tableName)
  }
  idx = grep(pattern, tableInfo$COLUMN_NAME, ignore.case=ignore.case, value=FALSE, invert=invert)
  
  if (names.only) 
    return(tableInfo[idx, "COLUMN_NAME"])
  else
    return(tableInfo[idx, ])
}


isCharacterColumn <- function (tableInfo, columnName) {
  is_column_char = getCharacterColumns(tableInfo, names.only=TRUE, include=columnName)
  return (ifelse(length(is_column_char) == 1, TRUE, FALSE))
}


isNumericColumn <- function (tableInfo, columnName) {
  is_column_numeric = getNumericColumns(tableInfo, names.only=TRUE, include=columnName)
  return (ifelse(length(is_column_numeric) == 1, TRUE, FALSE))
}


isDateTimeColumn <- function (tableInfo, columnName) {
  is_column_datetime = getDateTimeColumns(tableInfo, names.only=TRUE, include=columnName)
  return (ifelse(length(is_column_datetime) == 1, TRUE, FALSE))
}


includeExcludeColumns <- function (tableInfo, include, except) {
  result = tableInfo
  
  if(!is.null(include))
    result = result[result$COLUMN_NAME %in% include,]
  
  if(!is.null(except)) 
    result = result[!result$COLUMN_NAME %in% except,]
  
  return(result)
}

 
getColumns <- function (tableInfo, types, names.only, include, except) {
  result = tableInfo[tableInfo$TYPE_NAME %in% types,]
  
  result = includeExcludeColumns(result, include, except)
  
  if (names.only) 
    return(result[,"COLUMN_NAME"])
  else
    return(result)
}


makeWhereClause <- function (where) {
  
  if(is.null(where))
    where_clause = " "
  else
    where_clause = paste(" WHERE", where, " ")
  
  return(where_clause)
}


makeOrderByClause <- function (order) {
  if (is.null(order))
    orderby_clause = " "
  else
    orderby_clause = paste(" ORDER BY", paste(order, collapse=", "))
  
  return (orderby_clause)
}


makeLimitClause <- function (top) {
  if (is.null(top)) 
    limit_clause = " "
  else
    limit_clause = paste(" LIMIT", top)
  
  return (limit_clause)
}