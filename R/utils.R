#' Compute columnwise statistics on Aster table.
#' 
#' For table compute column statistics in Aster and augment data frame structure
#' obtained with \code{\link{sqlColumns}} with columns containing computed statistics.
#' 
#' Computes columns statistics for all or specified table columns and adds them
#' to the data frame with basic ODBC table metadata obtained with \code{\link{sqlColumns}}.
#' Computed statistics include counts of all, non-null, distinct values; statistical
#' summaries of maximum, minimum, mean, standard deviation, median (50th percentile), mode
#' (optional), interquartile range, and desired percentiles. Each computed statistic adds
#' a column to ODBC metadata data frame.
#' 
#' @param channel object as returned by \code{\link{odbcConnect}}.
#' @param tableName name of the table in Aster.
#' @param include a vector of column names to include. Output never contains attributes other than in the list.
#' @param except a vector of column names to exclude. Output never contains attributes from the list.
#' @param modeValue logical indicates if mode values should be computed. Default is FALSE.
#' @param percentiles list of percentiles (integers between 0 and 100) to collect (always collects 25th and 75th 
#'   for IQR calculation). There is no penalty in specifying more percentiles as they get calculated in a single call 
#'   for each column - no matter how many different values are requested.
#' @param where SQL WHERE clause limiting data from the table (use SQL as if in WHERE clause but omit keyword WHERE).
#' @param mock logical: if TRUE returns pre-computed table statistics for tables \code{pitching} or \code{batting}, only.
#' @return data frame returned by \code{\link{sqlColumns}} with additional columns:
#' \describe{
#'   \item{total_count}{total row count - the same for each table column}
#'   \item{distinct_count}{distinct values count}
#'   \item{not_null_count}{not null count}
#'   \item{minimum}{minimum value (numerical data types only)}
#'   \item{maximum}{maximum value (numerical data types only)}
#'   \item{average}{mean (numerical data types only)}
#'   \item{deviation}{standard deviation (numerical data types only)}
#'   \item{percentiles}{defaults: 0,5,10,25,50,75,90,95,100. Always adds percentiles 25, 50 (median), 75}
#'   \item{IQR}{interquartile range is the 1st Quartile subtracted from the 3rd Quartile}
#'   \item{minimum_str}{minimum string value (character data types only)}
#'   \item{maximum_str}{maximum string value (character data types only)}
#'   \item{mode}{mode value (optional)}
#'   \item{mode_count}{mode count (optional)}
#' }
#' @seealso \link{sqlColumns}
#' @export
#' @examples 
#' \donttest{
#' pitchingInfo = getTableSummary(channel=conn, 'pitching_enh')
#' # list all table columns
#' pitchingInfo$COLUMN_NAME
#' 
#' # compute statistics on subset of baseball data after 1999
#' battingInfo = getTableSummary(channel=conn, 'batting_enh', 
#'                               where='yearid between 2000 and 2013')
#'                               
#' # compute statistics for certain columns including each percentile from 1 to 99
#' pitchingInfo = getTableSummary(channel=conn, 'pitching_enh',
#'                               include=c('h', 'er', 'hr', 'bb', 'so'),
#'                               percentiles=seq(1,99))
#' # list data frame column names to see all computed statistics
#' names(pitchingInfo)
#'                              
#' # compute statitics on all numeric columns except certain columns
#' teamInfo = getTableSummary(channel=conn, 'teams_enh', 
#'                            include=getNumericColumns(sqlColumns(conn, 'teams_enh')),
#'                            except=c('lgid', 'teamid', 'playerid', 'yearid', 'decadeid'))                                                                                              
#' }
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
  
  if (nrow(table_info) == 0) stop(paste("No columns of any kind found in the table '", tableName, "'"))
  
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
                        paste0("SELECT COUNT(*) cnt FROM ", tableName, where_clause))
  if (!is.data.frame(total_rows) || nrow(total_rows) != 1)
    stop(paste("Not a valid sql to count total number of rows in the table '", tableName, "'"))
  
  total_count = total_rows[[1,1]]
  table_info[, "total_count"] = total_count
  
  # Loop over numeric columns
  for(column_name in getNumericColumns(table_info)) {
    
    # Compute SELECT aggregate statistics on each numeric column
    column_stats = sqlQuery(channel,
                            paste0("SELECT cast(count(distinct\"", column_name,"\") as bigint) as distinct_count, ",
                                   "       cast(count(\"", column_name, "\") as bigint) as not_null_count, ",
                                   "       min(\"", column_name,"\") as minimum, ",
                                   "       max(\"", column_name,"\") as maximum, ",
                                   "       avg(\"", column_name,"\"::numeric) as average, ",
                                   "       stddev(\"", column_name,"\"::numeric) as deviation ",
                                   "  FROM ", tableName, where_clause))
    if (!is.data.frame(column_stats) || nrow(column_stats) != 1)
      stop(paste("Not a valid sql to compute stats on numeric column '", column_name, "'"))
    
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
                            paste0("SELECT cast(count(distinct\"", column_name, "\") as bigint) as distinct_count, ",
                                   "       cast(count(\"", column_name, "\") as bigint) as not_null_count, ",
                                   "       min(\"", column_name, "\") as minimum, ",
                                   "       max(\"", column_name, "\") as maximum ",
                                   "  FROM ", tableName, where_clause)
    )
    if (!is.data.frame(column_stats) || nrow(column_stats) != 1)
      stop(paste("Not a valid sql to compute stats on non-numeric column '", column_name, "'"))
    
    column_idx = which(table_info$COLUMN_NAME == column_name)
    table_info[column_idx, "distinct_count"] = column_stats[[1,"distinct_count"]]
    table_info[column_idx, "not_null_count"] = column_stats[[1,"not_null_count"]]
    table_info[column_idx, "null_count"] = total_count - column_stats[[1,"not_null_count"]]
    table_info[column_idx, "minimum_str"] = as.character(column_stats[[1,"minimum"]])
    table_info[column_idx, "maximum_str"] = as.character(column_stats[[1,"maximum"]])
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

#' Validation of lookup data in database
#' 
#' Convinience function to test if table has indeed a lookup column in another table.
#' 
#' @param channel object as returned by \code{\link{odbcConnect}}.
#' @param tableName data table name.
#' @param columnName column name in data table.
#' @param lookupTable lookup table name.
#' @param lookupColumn column name in lookup table.
#' @param ignoreCase ignore case when comparing data. May produce slower results, especially on 
#'   partitioned data.
#'   
#' @return TRUE if lookup relationship between tables and columns holds, otherwise FALSE
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
    col_indices = c(col_indices, "minimum", "maximum", "average", "deviation")
    if("mode" %in% names(tableInfo)) {
      col_indices = c(col_indices, "mode")
    } 
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