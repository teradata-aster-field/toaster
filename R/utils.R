#' Augment Column table with column statistics computed in Aster
#' 
#' Computes columns statistics for all or specified table columns and adds them
#' to the data frame with basic ODBC table metadata obtained with \code{\link{sqlColumns}}.
#' 
#' @param channel object as returned by \code{\link{odbcConnect}}.
#' @param tableName table name.
#' @param include a vector of column names to include. Output never contains attributes other than in the list.
#' @param except a vector of column names to exclude. Output never contains attributes from the list.
#' @param modeValue logical indicates if mode values should be computed.
#' @param percentiles list of percentiles (integers between 0 and 100) to collect (always collects 25th and 75th 
#'   for IQR calculation). There is no penalty in specifying more percentiles as they get calculated in a single call 
#'   for each column - no matter how many different values are requested.
#' @param where SQL WHERE clause limiting data from the table (use SQL as if in WHERE clause but omit keyword WHERE).
#' @param mock logical: if TRUE returns pre-computed table statistics for tables \code{pitching} or \code{batting}, only.
#' @seealso \link{sqlColumns}
#' @export
#' 
getTableSummary <- function (channel, tableName, include = NULL, except = NULL, 
                             modeValue = FALSE,
                             percentiles = c(0,5,10,25,50,75,90,95,100),
                             where = NULL, mock = FALSE) {
  
  if (mock) {
    if (substr(tableName, nchar(tableName)-nchar('pitching')+1, nchar(tableName))=='pitching') {
      table_info = dget("pitchingInfo.dat")
    }else if (substr(tableName, nchar(tableName)-nchar('batting')+1, nchar(tableName))=='batting') {
      table_info = dget("battingInfo.dat")
    }else {
      stop("Test sql with 'getTableSummary' only for 'batting' or 'pitching' tables.")
    }
  }else {
    table_info = sqlColumns(channel, tableName)
  }
  
  table_info = includeExcludeColumns(table_info, include, except)
  
  # check if at least one column found
  if (nrow(table_info) == 0) stop(paste("No columns specified found in the table '", tableName, "'"))
  
  # no database access if mock is TRUE
  if (mock) {
    return (table_info)
  }
  
  where_clause = makeWhereClause(where)
  
  # percentiles
  # always add 50 (median) and 25, 75 for IQR computations
  if (is.null(percentiles) | !is.numeric(percentiles)) {
    percentiles = c(25, 50, 75)
  }else {
    percentiles = union(percentiles, c(25, 50, 75)) 
  }
  if (any(percentiles < 0 | percentiles > 100)) {
    stop (paste("Invalid percentile value(s) passed (below 0 or above 100): ", percentiles))
  }
  
  percentileNames = paste0(percentiles, "%")
  names(percentileNames) = percentiles
  percentileStr = paste(percentiles, collapse=",")
  
  # Create data frame columns for table statistics
  table_info[c("total_count","distinct_count","not_null_count")] = as.integer(NA)
  table_info[c("minimum","maximum","average","deviation", percentileNames, "IQR")] = as.numeric(NA)
  table_info[c("minimum_str","maximum_str")] = as.character(NA)
  
  # Total rows
  total_rows = sqlQuery(channel,
                        paste0("SELECT COUNT(*) cnt FROM ", tableName, where_clause)
  )
  total_count = total_rows[[1,1]]
  table_info[, "total_count"] = total_count
  
  # Loop over numeric columns
  for(column_name in getNumericColumns(table_info)) {
    
    # Compute SELECT aggregate statistics on each numeric column
    column_stats = sqlQuery(channel,
                            paste0("SELECT count(distinct\"", column_name,"\") as distinct_count, ",
                                   "       count(\"", column_name, "\") as not_null_count, ",
                                   "       min(\"", column_name,"\") as minimum, ",
                                   "       max(\"", column_name,"\") as maximum, ",
                                   "       avg(cast(\"", column_name,"\" as bigint)) as average, ",
                                   "       stddev(cast(\"", column_name,"\" as bigint)) as deviation ",
                                   "  FROM ", tableName, where_clause)
    )
    column_idx = which(table_info$COLUMN_NAME == column_name)
    
    table_info[column_idx, "distinct_count"] = column_stats[[1,"distinct_count"]]
    table_info[column_idx, "not_null_count"] = column_stats[[1,"not_null_count"]]
    table_info[column_idx, "null_count"] = total_count - column_stats[[1,"not_null_count"]]
    table_info[column_idx, "minimum"] = column_stats[[1,"minimum"]]
    table_info[column_idx, "maximum"] = column_stats[[1,"maximum"]]
    table_info[column_idx, "average"] = column_stats[[1,"average"]]
    table_info[column_idx, "deviation"] = column_stats[[1,"deviation"]]
    
    # compute all percentiles at once with SQL/MR approximate percentile function
    presults = sqlQuery(channel,
                           paste0("SELECT * FROM approxPercentileReduce(
                                  ON (
                                  SELECT * FROM approxPercentileMap(
                                  ON  ( SELECT * FROM " , tableName, where_clause, " ) ",
                                  " TARGET_COLUMN( '",column_name,"' )
                                  ERROR( 1 )
                                  )
                                  )
                                  PARTITION BY 1
                                  PERCENTILE( ", percentileStr, " ))")
                           )
    
    if (nrow(presults) > 0) {
      for (tile in 1:nrow(presults)) {
        ptile = as.character(presults[[tile, "percentile"]])
        ptileValue = presults[[tile, "value"]]
        table_info[column_idx, percentileNames[ptile]] = ptileValue
      }
      
      table_info[column_idx, "IQR"] = presults[[which(presults$percentile==75),"value"]] -
        presults[[which(presults$percentile==25),"value"]]
      
    }
  }
  
  # Loop over non-numeric columns 
  non_numeric_columns = c(getCharacterColumns(table_info),
                          getDateTimeColumns(table_info))
  for(column_name in non_numeric_columns) {
    column_stats = sqlQuery(channel,
                            paste0("SELECT count(distinct\"", column_name, "\") as distinct_count, ",
                                   "       count(\"", column_name, "\") as not_null_count, ",
                                   "       min(\"", column_name, "\") as minimum, ",
                                   "       max(\"", column_name, "\") as maximum ",
                                   "  FROM ", tableName, where_clause)
    )
    column_idx = which(table_info$COLUMN_NAME == column_name)
    table_info[column_idx, "distinct_count"] = column_stats[[1,"distinct_count"]]
    table_info[column_idx, "not_null_count"] = column_stats[[1,"not_null_count"]]
    table_info[column_idx, "null_count"] = total_count - column_stats[[1,"not_null_count"]]
    table_info[column_idx, "minimum_str"] = column_stats[[1,"minimum"]]
    table_info[column_idx, "maximum_str"] = column_stats[[1,"maximum"]]
  }
  
  # Compute modes
  if(modeValue) {
    for(column_name in table_info$COLUMN_NAME) {
      mode = sqlQuery(channel,
                      paste0("SELECT \"", column_name, "\" val, count(*) cnt ",
                             "  FROM ", tableName, where_clause,
                             " GROUP BY 1 ORDER BY 2 DESC LIMIT 1")
      )
      column_idx = which(table_info$COLUMN_NAME == column_name)
      table_info[column_idx, "mode"] = toString(mode[[1,"val"]])
      table_info[column_idx, "mode_count"] = mode[[1, "cnt"]]
    }
    
  }
  
  return(table_info)
  
}

#' convinience function to test if table has indeed a lookup column in another table.
#' 
#' @param channel object as returned by \code{\link{odbcConnect}}.
#' @param tableName data table name.
#' @param columnName column name in data table.
#' @param lookupTable lookup table name.
#' @param lookupColumn column name in lookup table.
#' @param ignoreCase ignore case when comparing data. May produce slower results, especially on 
#'   partitioned data.
#'
#' @export
isLookupForColumn  <- function(channel, tableName, columnName, lookupTable, lookupColumn=columnName,
                               ignoreCase=FALSE) {
  
  # ignore case or not
  if (ignoreCase) {
    columnName = paste0("lower(", columnName, ")")
    lookupColumn = paste0("lower(", lookupColumn, ")")
  }
  
  # check total number of distinct values in the data set
  result = sqlQuery(channel, 
                    paste0("SELECT COUNT(DISTINCT ", columnName, ") cnt  
                           FROM ", tableName)
                    )
  totalValues = result$cnt[[1]]
  
  # check number of distinct values in lookup table 
  result = sqlQuery(channel,
                    paste0("SELECT COUNT(DISTINCT ", columnName, ") cnt 
                              FROM ", tableName, 
                           " WHERE ", columnName, " IN (SELECT ", lookupColumn, " FROM ", lookupTable, ")")
  )
  resolvedValues = result$cnt[[1]]
  
  result = sqlQuery(channel, 
                    paste0("SELECT COUNT(DISTINCT ", lookupColumn, ") cnt_dist, 
                                   COUNT(", lookupColumn, ") cnt 
                              FROM ", lookupTable)
  )
  lookupValues = result$cnt_dist[[1]]
  lookupTotalValues = result$cnt[[1]]
  
  # Report results
  print(paste0("Total values in ", tableName, ".", columnName, ": ", totalValues))
  print(paste0("Total values found in ", lookupTable, ".", lookupColumn, ": ", resolvedValues))
  print(paste0("Lookup table support is ", sprintf("%.2f",(resolvedValues/totalValues)*100.), "%"))
  if (lookupTotalValues > lookupValues) {
    print(paste0("WARNING: ", lookupTable, ".", lookupColumn, " contains ", 
                 lookupTotalValues - lookupValues, " duplicate values"))
  }
  
  return()
  
}

#' Invoke a Data Viewer
#' 
#' view computed column statistics in a spreadsheet-style viewer in R.
#' 
#' When both parameters \code{basic} and \code{percentiles} are FALSE view displays \emph{all} statistics.
#' 
#' @param tableInfo data frame with columns statistics to display.
#' @param include a vector of column names to include. Output never contains attributes other than in the list.
#' @param except a vector of column names to exclude. Output never contains attributes from the list.
#' @param basic logical: if TRUE display minimum, maximum, average, deviation and mode (if present)
#' @param percentiles logical: if TRUE display percentiles
#' 
#' @seealso \code{\link{getTableSummary}}
#' @export
viewTableSummary <- function(tableInfo, include=NULL, except=NULL, basic=FALSE, percentiles=FALSE) {
  
  if (missing(tableInfo)) return()
  
  tableInfo = includeExcludeColumns(tableInfo, include, except)
  
  col_indices = c("COLUMN_NAME")
  
  if(basic) {
    col_indices = c(col_indices, "minimum", "maximum", "average", "deviation", "mode")
  }
  
  if(percentiles) {
    ns = names(tableInfo)
    col_indices = c(col_indices, ns[grep("^[0-9]*%$",ns)])
  }
  
  if (length(col_indices) == 1) {
    col_indices = c(4,19:ncol(tableInfo))
  }
    
  View(tableInfo[, col_indices])
  
  return(1.0)
}

#' Joins two tables and lets use result in place of a table name.
#'
#' @param table1 name of 1st table to join (left position)
#' @param table2 name of 2d table to join (right position)
#' @param join type of join: \code{c('inner','left','right','full')}
#' @param alias1 alias for 1st table
#' @param alias2 alias for 2d table
#' @param joinColumns one or more columns to join tables on. These column names are to be found in both joined tables. 
#'   If tables have different column names to join on or condition is not simple equality then use parameter \code{joinCondition}
#' @param joinCondition explicitly defined join condition, \code{joinColumns} will be ignored.
#' @param select1 list of columns to select from 1st table
#' @param select2 list of columns to select from 2d table
#' @param alias table alias for join resultto use for resulting join
#' @param smart if TRUE then select list for join will be constructed by function to eliminate 
#'   duplicate columns (default is FALSE)
#' @param channel if \code{smart} is TRUE then required to access table info
#' @export
#' @examples
#' \donttest{
#' # 2-column inner join
#' joinTable('pitching','teams', c('teamid','yearid'))
#' # same with columns selected from both tables
#' joinTab joinTables('pitching', 'teams', 'inner', 'a', 'b', c('teamid','yearid'), 
#'                    select2=c('rank','g','w','l','divwin','wcwin','lgwin','wswin'), 
#'                    alias='myalis')
#' }
joinTables <- function(table1, table2, join='inner', alias1='a', alias2='b', joinColumns, joinCondition=NULL,
                       select1="*", select2=NULL, alias='t', smart=FALSE, channel=NULL) {
  
  if (!smart && !missing(select1) && !missing(select2) && length(select1)>=1 && 
        length(select2)>=1 && select1[[1]]=='*' && select1[[1]]==select2[[1]]) {
    stop("Both tables can't select ALL (*) columns when joined together.")
  }
  
  if (length(select1) > 1 & '*' %in% select1) {
    stop("Select list (1st table) contains '*' and other names which is not allowed.")
  }

  if (length(select2) > 1 & '*' %in% select2) {
    stop("Select list (2d table) contains '*' and other names which is not allowed.")
  }
  
  if (any(select2 %in% select1)) {
    stop("Select lists from joined tables can't contain same columns.")
  }
  
  join = match.arg(join, c('inner','left','right','full'))
  joinKeyword = list('INNER JOIN', 'LEFT JOIN', 'RIGHT JOIN', 'FULL OUTER JOIN')[match(join, c('inner','left','right','full'))]
  
  if (missing(joinCondition)) {
    joinCondition = paste0(alias1,'.',joinColumns, '=', alias2,'.',joinColumns, collapse=' AND ')
  }
  
  fromList = paste(table1, alias1, joinKeyword, table2, alias2, sep=' ')
  
  if (smart) {
    fromList = makeJoinColumnList(channel, table1, table2, select1, select2)
  }else {
    selectList = ifelse(missing(select2), 
                        paste0(alias1, '.', select1, collapse=', '),
                        paste(paste0(alias1, '.', select1, collapse=', '), 
                              paste0(alias2, '.', select2, collapse=', '), 
                              sep=', ')
    )
  }
  
  return(paste("( SELECT", selectList, "FROM", fromList, "ON (", joinCondition, ") )", alias))
}

makeJoinColumnList <- function(channel, table1, table2, select1, select2) {
  
  if (is.null(select1)) {
    select1 = character(0)
  } 
  if (is.null(select2)) {
    select2 = character(0)
  }
  
  if (select1 == "*") {
    list1 = NULL
  }else {
    list1 = select1
  }
  
  if (select2 == "*") {
    list2 = NULL
  }else {
    list2 = select2
  }
  tableInfo1 = sqlColumns(channel, table1)
  
  tableInfo1 = includeExcludeColumns(tableInfo1, list1, NULL)
  
  tableInfo2 = sqlColumns(channel, table2)
  
  tableInfo2 = includeExcludeColumns(tableInfo2, list2, NULL)
  
  return(union(tableInfo1$COLUMN_NAME, tableInfo2$COLUMN_NAME))
  
}