require(RODBC)

#' List Numeric Columns in Aster Table
#'
#' This function selects numeric Aster columns.
#' 
#' @param tableInfo data frame with all table columns, obtained by calling RODBC function \code{\link{sqlColumns}}
#' @param names.only boolean flag indicating if function returns list of column names or a data frame
#'   with columns info
#' @param include a vector of column names to include. Output is restricted to this list.
#' @param except a vector of column names to exclude. Output never contains names from this list.
#' 
getNumericColumns <- function (tableInfo, names.only=TRUE, include=NULL, except=NULL) {
  numeric_types = c('integer',
                    'numeric',
                    'bigint',
                    'smallint',
                    'real',
                    'double precision',
                    'serial',
                    'bigserial',
                    'float',
                    'decimal')
  
  return(getColumns(tableInfo, numeric_types, names.only, include, except))
}



#' List character table columns
#' 
getCharacterColumns <- function (tableInfo, names.only=TRUE, include=NULL, except=NULL) {
  char_types = c('varchar',
                 'char',
                 'character')
  
  return(getColumns(tableInfo, char_types, names.only, include, except))
}


#' List Date and Time Table Columns
#' 
getDateTimeColumns <- function (tableInfo, names.only=TRUE, include=NULL, except=NULL) {
  datetime_types = c('date', 
                     'timestamp without time zone', 
                     'timestamp with time zone',
                     'time without time zone',
                     'time with time zone')
  
  return(getColumns(tableInfo, datetime_types, names.only, include, except))
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


#' Helper function to restrict list of columns
#' 
includeExcludeColumns <- function (tableInfo, include, except) {
  result = tableInfo
  
  if(!is.null(include))
    result = result[result$COLUMN_NAME %in% include,]
  
  if(!is.null(except)) 
    result = result[!result$COLUMN_NAME %in% except,]
  
  return(result)
}

#' Helper Function
#' 
getColumns <- function (tableInfo, types, names.only, include, except) {
  result = tableInfo[tableInfo$TYPE_NAME %in% types,]
  
  result = includeExcludeColumns(result, include, except)
  
  if (names.only) 
    return(result[,"COLUMN_NAME"])
  else
    return(result)
}

#' Helper Function
#' 
makeWhereClause <- function (where) {
  
  if(is.null(where))
    where_clause = " "
  else
    where_clause = paste(" WHERE", where, " ")
  
  return(where_clause)
}