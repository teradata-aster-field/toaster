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
#' @param parallel logical: enable parallel calls to Aster database. This option requires parallel backend enabled and
#'   registered (see in examples). Parallel execution requires ODBC \code{channel} obtained only with 
#'   \code{\link{odbcDriverConnect}} function, not \code{\link{odbcConnect}}.
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
                             where = NULL, mock = FALSE, parallel = FALSE) {
  
  # check if parallel option is valid
  if (parallel && !getDoParRegistered())
    stop("Please register parallel backend appropriate to your platform to run with parallel=TRUE")
  
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
  table_info[c("total_count","distinct_count","not_null_count","null_count")] = as.integer(NA)
  table_info[c("minimum","maximum","average","deviation", percentileNames, "IQR")] = as.numeric(NA)
  table_info[c("minimum_str","maximum_str")] = as.character(NA)
  
  # Total rows
  total_rows = sqlQuery(channel,
                        paste0("SELECT COUNT(*) cnt FROM ", tableName, where_clause))
  if (!is.data.frame(total_rows) || nrow(total_rows) != 1)
    stop(paste("Not a valid sql to count total number of rows in the table '", tableName, "'"))
  
  total_count = total_rows[[1,1]]
  table_info[, "total_count"] = total_count
  
  # compute numeric metrics
  numeric_columns = getNumericColumns(table_info)
  
  if (length(numeric_columns) > 0) {
    metrics = computeNumericMetrics(channel, tableName, table_info, numeric_columns, 
                                    percentileNames, percentileStr, 
                                    where_clause, total_count, parallel)
    
    table_info[metrics$idx, 
               c("distinct_count", "not_null_count", "null_count", 
                 "minimum", "maximum", "average", "deviation", percentileNames, "IQR")] =
      metrics[, c("distinct_count", "not_null_count", "null_count", 
                  "minimum", "maximum", "average", "deviation", percentileNames, "IQR")]
  }
  
  
  # compute non-numeric metrics 
  non_numeric_columns = c(getCharacterColumns(table_info),
                          getDateTimeColumns(table_info))
  
  if (length(non_numeric_columns) > 0) {
    metrics = computeNonNumericMetrics(channel, tableName, table_info, non_numeric_columns, 
                                       where_clause, total_count, parallel)
    
    table_info[metrics$idx, c("distinct_count", "not_null_count", "null_count", 
                              "minimum_str", "maximum_str")] =
      metrics[, c("distinct_count", "not_null_count", "null_count", 
                  "minimum_str", "maximum_str")]
  }
  
  # Compute modes
  if(modeValue) {
    table_info = cbind(table_info, 
                       mode=character(length(table_info$COLUMN_NAME)), 
                       mode_count=integer(length(table_info$COLUMN_NAME)),
                       stringsAsFactors=FALSE)
    
    modes = computeModes(channel, tableName, table_info, where_clause, parallel)
    
    table_info[modes$idx, c("mode", "mode_count")] = modes[, c('mode_value', 'mode_count')]

  }
  
  return(table_info)
  
}

makeNumericMetrics <- function(idx, column_stats, total_count, presults, percentileNames) {
  df = data.frame(
             idx=idx,
             distinct_count=column_stats[[1,"distinct_count"]],
             not_null_count=column_stats[[1,"not_null_count"]],
             null_count=total_count - column_stats[[1,"not_null_count"]],
             minimum=column_stats[[1,"minimum"]],
             maximum=column_stats[[1,"maximum"]],
             average=column_stats[[1,"average"]],
             deviation=column_stats[[1,"deviation"]],
             stringsAsFactors=FALSE)
  
  if (nrow(presults) > 0) {
    for (tile in 1:nrow(presults)) {
      ptile = as.character(presults[[tile, "percentile"]])
      ptileValue = presults[[tile, "value"]]
      df[1, percentileNames[ptile]] = ptileValue
    }
    
    df[1, "IQR"] = presults[[which(presults$percentile==75),"value"]] -
      presults[[which(presults$percentile==25),"value"]]
  }
  
  return (df)
}

computeNumericMetrics <- function(channel, tableName, tableInfo, numeric_columns,
                                  percentileNames, percentileStr, where_clause, 
                                  total_count, parallel=FALSE) {
  
  metricsSql = 
    paste0("SELECT cast(count(distinct (%%%column__name%%%)) as bigint) as distinct_count, ",
           "       cast(count((%%%column__name%%%)) as bigint) as not_null_count, ",
           "       min((%%%column__name%%%)) as minimum, ",
           "       max((%%%column__name%%%)) as maximum, ",
           "       avg((%%%column__name%%%)::numeric) as average, ",
           "       stddev((%%%column__name%%%)::numeric) as deviation ",
           "  FROM ", tableName, where_clause)
  
  percentileSql = 
    paste0("SELECT * FROM approxPercentileReduce(
                                  ON (
                                  SELECT * FROM approxPercentileMap(
                                  ON  ( SELECT * FROM " , tableName, where_clause, " ) ",
           " TARGET_COLUMN( '(%%%column__name%%%)' )
                                  ERROR( 1 )
                                  )
                                  )
                                  PARTITION BY 1
                                  PERCENTILE( ", percentileStr, " ))")
  
  if (!parallel) {
    result = foreach(column_name = tableInfo$COLUMN_NAME, idx = seq_along(tableInfo$COLUMN_NAME),
                     .combine='rbind', .packages=c('RODBC')) %do% {
                       
               if (column_name %in% numeric_columns) {
                 
                 # Compute SELECT aggregate statistics on each numeric column
                 column_stats = sqlQuery(channel, gsub('(%%%column__name%%%)', column_name, metricsSql, fixed=TRUE), 
                                         stringsAsFactors = FALSE)
                         
                 # compute all percentiles at once with SQL/MR approximate percentile function
                 presults = sqlQuery(channel, gsub('(%%%column__name%%%)', column_name, percentileSql, fixed=TRUE), 
                                     stringsAsFactors = FALSE)
                         
                 makeNumericMetrics(idx, column_stats, total_count, presults, percentileNames)
               }  
            }
  }else {
    # parallel mode compute
    result = foreach(column_name = tableInfo$COLUMN_NAME, idx = seq_along(tableInfo$COLUMN_NAME),
                     .combine='rbind', .packages=c('RODBC'), .inorder=FALSE) %dopar% {
                       
               if (column_name %in% numeric_columns) {
                         
                 parChan = odbcReConnect(channel)
                 # Compute SELECT aggregate statistics on each numeric column
                 column_stats = sqlQuery(parChan, gsub('(%%%column__name%%%)', column_name, metricsSql, fixed=TRUE), 
                                         stringsAsFactors = FALSE)
                         
                 # compute all percentiles at once with SQL/MR approximate percentile function
                 presults = sqlQuery(parChan, gsub('(%%%column__name%%%)', column_name, percentileSql, fixed=TRUE), 
                                     stringsAsFactors = FALSE)
                 close(parChan)
                         
                 makeNumericMetrics(idx, column_stats, total_count, presults, percentileNames)
              }  
            }
  }

  return (result)
  
}

makeNonNumericMetrics <- function(idx, column_stats, total_count) {
  data.frame(idx=idx, 
             distinct_count=column_stats[[1,"distinct_count"]],
             not_null_count=column_stats[[1,"not_null_count"]],
             null_count=total_count - column_stats[[1,"not_null_count"]],
             minimum_str=as.character(column_stats[[1,"minimum"]]),
             maximum_str=as.character(column_stats[[1,"maximum"]]),
             stringsAsFactors=FALSE)
}

computeNonNumericMetrics <- function(channel, tableName, tableInfo, non_numeric_columns, 
                                     where_clause, total_count, parallel=FALSE) {
  
  sql =
    paste0("SELECT cast(count(distinct (%%%column__name%%%)) as bigint) as distinct_count, ",
           "       cast(count((%%%column__name%%%)) as bigint) as not_null_count, ",
           "       min((%%%column__name%%%)) as minimum, ",
           "       max((%%%column__name%%%)) as maximum ",
           "  FROM ", tableName, where_clause)
  
  if (!parallel) {
    result = foreach(column_name = tableInfo$COLUMN_NAME, idx = seq_along(tableInfo$COLUMN_NAME),
                     .combine='rbind', .packages=c('RODBC')) %do% {
                       
               if (column_name %in% non_numeric_columns) {
                 column_stats = sqlQuery(channel, gsub('(%%%column__name%%%)', column_name, sql, fixed=TRUE), 
                                         stringsAsFactors = FALSE)
                         
                 makeNonNumericMetrics(idx, column_stats, total_count) 
               }
            }
  }else {
    # parallel mode compute
    result = foreach(column_name = tableInfo$COLUMN_NAME, idx = seq_along(tableInfo$COLUMN_NAME),
                     .combine='rbind', .packages=c('RODBC'), .inorder=FALSE) %dopar% {
      
              if (column_name %in% non_numeric_columns) {
                parChan = odbcReConnect(channel)
                column_stats = sqlQuery(parChan, gsub('(%%%column__name%%%)', column_name, sql, fixed=TRUE), 
                                                 stringsAsFactors = FALSE)
                close(parChan)
                         
                makeNonNumericMetrics(idx, column_stats, total_count) 
              }
            }
  }
  
  return (result)
}

makeMode <- function(idx, mode) {
  data.frame(idx=idx, mode_value=mode[[1,"val"]], mode_count=mode[[1, "cnt"]], 
             stringsAsFactors=FALSE)
}

computeModes <- function(channel, tableName, tableInfo, where_clause, parallel=FALSE) {
  
  sql =
    paste0("SELECT CAST((%%%column__name%%%) as varchar) val, count(*) cnt ",
               "  FROM ", tableName, where_clause,
               " GROUP BY 1 ORDER BY 2 DESC LIMIT 1")
  
  if (!parallel) {
    result = foreach(column_name = tableInfo$COLUMN_NAME, idx = seq_along(tableInfo$COLUMN_NAME),
                     .combine='rbind', .packages=c('RODBC')) %do% {      
              
              mode = sqlQuery(channel, sub('(%%%column__name%%%)', column_name, sql, fixed=TRUE), 
                              stringsAsFactors = FALSE)
              
              makeMode(idx, mode)
             }
  }else {
    # parallel mode compute 
    result = foreach(column_name = tableInfo$COLUMN_NAME, idx = seq_along(tableInfo$COLUMN_NAME),
                     .combine='rbind', .packages=c('RODBC'), .inorder=FALSE) %dopar% {
              
              parChan = odbcReConnect(channel)
              mode = sqlQuery(parChan, sub('(%%%column__name%%%)', column_name, sql, fixed=TRUE), 
                              stringsAsFactors = FALSE)
              close(parChan)
              
              makeMode(idx, mode)
            }
  }
  
  return (result)
}

#' Invoke a Data Viewer on table statistics.
#' 
#' view computed column statistics in a spreadsheet-style viewer in R.
#' 
#' When both parameters \code{basic} and \code{percentiles} are FALSE view displays \emph{all} statistics.
#' 
#' @param tableInfo data frame with columns statistics to display.
#' @param types vector with types of columns to include: numerical (\code{"numeric"}), character (\code{"character"} or 
#'   date/time (\code{"temporal"})
#' @param include a vector of column names to include. Output never contains attributes other than in the list.
#' @param except a vector of column names to exclude. Output never contains attributes from the list.
#' @param basic logical: if TRUE display minimum, maximum, average, deviation and mode (if present)
#' @param percentiles logical: if TRUE display percentiles
#' 
#' @seealso \code{\link{getTableSummary}}
#' @export
#' @examples 
#' \donttest{
#' pitchingInfo = getTableSummary(channel=conn, 'pitching_enh')
#' viewTableSummary(pitchingInfo, percentiles=TRUE)
#' 
#' viewTableSummary(pitchingInfo, types=c("numeric", "temporal"))
#' }
viewTableSummary <- function(tableInfo, types=NULL,
                             include=NULL, except=NULL, basic=FALSE, 
                             percentiles=FALSE) {
    
  if (missing(tableInfo)) return()
  
  tableInfo = includeExcludeColumns(tableInfo, include, except)
  
  col_indices = c("COLUMN_NAME", "TYPE_NAME")
  
  if(basic) {
    col_indices = c(col_indices, "total_count", "distinct_count", "not_null_count", "null_count")  
  }else {
    col_indices = c(col_indices, "total_count", "distinct_count", "not_null_count", "null_count",
                    "minimum", "maximum", "average", "deviation",
                    "minimum_str", "maximum_str")
  }
  
  if("mode" %in% names(tableInfo)) {
    col_indices = c(col_indices, "mode")
  }
  
  if(percentiles) {
    ns = names(tableInfo)
    col_indices = c(col_indices, ns[grep("^[0-9]*%$",ns)])
  }
  
  if (!is.null(types) && length(types) > 0) {
    typeNames = getTypes(types)
    row_indices = tableInfo$TYPE_NAME %in% typeNames
  }else 
    row_indices = !is.na(tableInfo$TYPE_NAME)
    
  View(tableInfo[row_indices, col_indices])
  
  return(1.0)
}

getColumnValues <- function(conn, tableName, columnName, where = NULL, mock = FALSE) {
  
  where_clause = makeWhereClause(where)
  
  if (mock) {
    if (columnName == 'lgid') 
      return (c('AL','NL'))
    if (columnName == 'teamid')
      return (c('NYY', 'TEX', 'BAL', 'TOR'))
    if (columnName == 'decadeid')
      return (c('1990', '2000', '2010'))
    if (columnName == 'non_compliant')
      return (c('+', '-'))
    if (columnName == 'no_values')
      return (character(0))
  }else {
    sql = paste0("SELECT DISTINCT ", columnName, " values FROM ", tableName, where_clause)
    
    return (sqlQuery(conn, sql, stringsAsFactors=FALSE)[, 'values'])
  }
  
  
}

# TODO: make part of utility convinience set of functions
grantExecuteOnFunction <- function(conn, name='%', owner='db_superuser', user) {
  
  if (missing(user)) {
    stop("User name to grant execute permission is missing.")
  }
  
  sql = paste0("select * from nc_system.nc_all_sqlmr_funcs where funcname like '", name, "' and funcowner = '",
               owner, "'")
  
  func_list = sqlQuery(conn, sql)
  
  if (!('funcname' %in% names(func_list)) || length(func_list$funcname) == 0) {
    stop("No functions to grant execute permission to.")
  }
  
  for(func_name in func_list$funcname){
    sql = paste0("grant execute on function ", func_name, " to ", user)
    # print(sql)
    result = sqlQuery(conn, sql)
  }
}