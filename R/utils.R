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
#' @param parallel logical: enable parallel calls to Aster database. This option requires parallel 
#'   backend enabled and registered (see in examples). Parallel execution requires ODBC \code{channel} 
#'   obtained without explicit password: either with \code{\link{odbcConnect}(dsn)} or 
#'   \code{\link{odbcDriverConnect}} calls, but not with \code{\link{odbcConnect}(dsn, user, password)}.
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
#' if(interactive()){
#' # initialize connection to Lahman baseball database in Aster 
#' conn = odbcDriverConnect(connection="driver={Aster ODBC Driver};
#'                          server=<dbhost>;port=2406;database=<dbname>;uid=<user>;pwd=<pw>")
#' 
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
#'                    include=getNumericColumns(sqlColumns(conn, 'teams_enh')),
#'                    except=c('lgid', 'teamid', 'playerid', 'yearid', 'decadeid'))
#' }
getTableSummary <- function (channel, tableName, include = NULL, except = NULL, 
                             modeValue = FALSE,
                             percentiles = c(5,10,25,50,75,90,95,100),
                             where = NULL, mock = FALSE, parallel = FALSE) {
  
  # check if parallel option is valid
  if (parallel && !getDoParRegistered())
    stop("Please register parallel backend appropriate to your platform to run with parallel=TRUE")
  
  tableName = normalizeTableName(tableName)
  
  if (mock) {
    if (substr(tableName, nchar(tableName)-nchar('pitching')+1, nchar(tableName))=='pitching') {
      table_info = dget("_pitchingInfo.dat")
    }else if (substr(tableName, nchar(tableName)-nchar('batting')+1, nchar(tableName))=='batting') {
      table_info = dget("_battingInfo.dat")
    }else {
      stop("Test sql with 'getTableSummary' only for 'batting' or 'pitching' tables.")
    }
  }else {
    table_info = sqlColumns(channel, tableName)
  }
  
  if (nrow(table_info) == 0) stop(paste0("No columns of any kind found in the table '", tableName, "'"))
  
  table_info = includeExcludeColumns(table_info, include, except)
  
  # check if at least one column found
  if (nrow(table_info) == 0) stop(paste0("No columns specified found in the table '", tableName, "'"))
  
  where_clause = makeWhereClause(where)
  
  # percentiles
  # always add 50 (median) and 25, 75 for IQR computations
  if (is.null(percentiles) || !is.numeric(percentiles)) {
    percentiles = c(25, 50, 75)
  }else {
    percentiles = union(percentiles, c(25, 50, 75)) 
  }
  out_of_range = percentiles < 0 | percentiles > 100
  if (any(out_of_range)) {
    stop(paste("Invalid percentile value(s) passed (below 0 or above 100):", percentiles[out_of_range]))
  }
  
  # no database access if mock is TRUE
  if (mock) {
    return (table_info)
  }
  
  percentileNames = paste0(percentiles, "%")
  percentileStrNames = paste0(percentiles, "%str")
  names(percentileNames) = percentiles
  names(percentileStrNames) = percentiles
  percentileStr = paste(percentiles, collapse=",")
  
  # Create data frame columns for table statistics
  table_info[c("total_count","distinct_count","not_null_count","null_count")] = as.integer(NA)
  table_info[c("minimum","maximum","average","deviation", percentileNames, "IQR")] = as.numeric(NA)
  table_info[c("minimum_str","maximum_str", "average_str", percentileStrNames)] = as.character(NA)
  
  # Total rows
  total_rows = toaSqlQuery(channel, paste0("SELECT COUNT(*) cnt FROM ", tableName, where_clause))
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
  
  
  # compute character metrics 
  character_columns = getCharacterColumns(table_info)
  
  if (length(character_columns) > 0) {
    metrics = computeCharacterMetrics(channel, tableName, table_info, character_columns, 
                                       where_clause, total_count, parallel)
    
    table_info[metrics$idx, c("distinct_count", "not_null_count", "null_count", 
                              "minimum_str", "maximum_str")] =
      metrics[, c("distinct_count", "not_null_count", "null_count", 
                  "minimum_str", "maximum_str")]
  }
  
  # compute temporal metrics
  temporal_columns = getTemporalColumns(table_info)
  
  if (length(temporal_columns) > 0) {
    metrics = computeTemporalMetrics(channel, tableName, table_info, temporal_columns,
                                     percentiles, percentileNames, percentileStrNames, percentileStr, 
                                     where_clause, total_count, parallel)
    
    table_info[metrics$idx, c("distinct_count", "not_null_count", "null_count",
                              "minimum_str", "maximum_str", "average_str", 
                              percentileNames, "IQR", percentileStrNames)] =
      metrics[, c("distinct_count", "not_null_count", "null_count",
                  "minimum_str", "maximum_str", "average_str", 
                  percentileNames, "IQR", percentileStrNames)]
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
  }else {
    for (ptileName in percentileNames)
      df[1, ptileName] = NA
    df[1, "IQR"] = NA
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
  
  # non-functional: eliminates NOTE 'no visible binding for global variable'
  column_name = idx = NULL
  
  if (!parallel) {
    result = foreach(column_name = tableInfo$COLUMN_NAME, idx = seq_along(tableInfo$COLUMN_NAME),
                     .combine='rbind', .packages=c('RODBC')) %do% {
                       
               if (column_name %in% numeric_columns) {
                 
                 # Compute SELECT aggregate statistics on each numeric column
                 column_stats = toaSqlQuery(channel, gsub('(%%%column__name%%%)', column_name, metricsSql, 
                                                          fixed=TRUE), stringsAsFactors = FALSE)
                         
                 # compute all percentiles at once with SQL/MR approximate percentile function
                 presults = toaSqlQuery(channel, gsub('(%%%column__name%%%)', column_name, percentileSql, 
                                                      fixed=TRUE), stringsAsFactors = FALSE)
                         
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
                 column_stats = toaSqlQuery(parChan, gsub('(%%%column__name%%%)', column_name, metricsSql, 
                                                          fixed=TRUE), 
                                         stringsAsFactors = FALSE, closeOnError=TRUE)
                         
                 # compute all percentiles at once with SQL/MR approximate percentile function
                 presults = toaSqlQuery(parChan, gsub('(%%%column__name%%%)', column_name, percentileSql, 
                                                      fixed=TRUE), 
                                     stringsAsFactors = FALSE, closeOnError=TRUE)
                 close(parChan)
                         
                 makeNumericMetrics(idx, column_stats, total_count, presults, percentileNames)
              }  
            }
  }

  return (result)
  
}

makeCharacterMetrics <- function(idx, column_stats, total_count) {
  data.frame(idx=idx, 
             distinct_count=column_stats[[1,"distinct_count"]],
             not_null_count=column_stats[[1,"not_null_count"]],
             null_count=total_count - column_stats[[1,"not_null_count"]],
             minimum_str=as.character(column_stats[[1,"minimum"]]),
             maximum_str=as.character(column_stats[[1,"maximum"]]),
             stringsAsFactors=FALSE)
}

computeCharacterMetrics <- function(channel, tableName, tableInfo, character_columns, 
                                     where_clause, total_count, parallel=FALSE) {
  
  sql =
    paste0("SELECT cast(count(distinct (%%%column__name%%%)) as bigint) as distinct_count, ",
           "       cast(count((%%%column__name%%%)) as bigint) as not_null_count, ",
           "       min((%%%column__name%%%)) as minimum, ",
           "       max((%%%column__name%%%)) as maximum ",
           "  FROM ", tableName, where_clause)
  
  # non-functional: eliminates NOTE 'no visible binding for global variable'
  column_name = idx = NULL 
  
  if (!parallel) {
    result = foreach(column_name = tableInfo$COLUMN_NAME, idx = seq_along(tableInfo$COLUMN_NAME),
                     .combine='rbind', .packages=c('RODBC')) %do% {
                       
               if (column_name %in% character_columns) {
                 column_stats = toaSqlQuery(channel, gsub('(%%%column__name%%%)', column_name, sql, fixed=TRUE), 
                                         stringsAsFactors = FALSE)
                         
                 makeCharacterMetrics(idx, column_stats, total_count) 
               }
            }
  }else {
    # parallel mode compute
    result = foreach(column_name = tableInfo$COLUMN_NAME, idx = seq_along(tableInfo$COLUMN_NAME),
                     .combine='rbind', .packages=c('RODBC'), .inorder=FALSE) %dopar% {
      
              if (column_name %in% character_columns) {
                parChan = odbcReConnect(channel)
                column_stats = toaSqlQuery(parChan, gsub('(%%%column__name%%%)', column_name, sql, fixed=TRUE), 
                                                 stringsAsFactors = FALSE, closeOnError=TRUE)
                close(parChan)
                         
                makeCharacterMetrics(idx, column_stats, total_count) 
              }
            }
  }
  
  return (result)
}

makeTemporalMetrics <- function(idx, column_stats, total_count, presults, percentileNames, percentileStrNames) {
  df = data.frame(
    idx=idx,
    distinct_count=column_stats[[1,"distinct_count"]],
    not_null_count=column_stats[[1,"not_null_count"]],
    null_count=total_count - column_stats[[1,"not_null_count"]],
    minimum_str=column_stats[[1,"minimum"]],
    maximum_str=column_stats[[1,"maximum"]],
    average_str=column_stats[[1,"average"]],
    stringsAsFactors=FALSE)
  
  if (nrow(presults) > 0) {
    for (tile in 1:nrow(presults)) {
      ptile = as.character(presults[[tile, "percentile"]])
      ptileValue = presults[[tile, "epoch"]]
      df[1, percentileNames[ptile]] = ptileValue
      
      ptileValue = presults[[tile, "value"]]
      df[1, percentileStrNames[ptile]] = ptileValue
    }
    
    df[1, "IQR"] = presults[[which(presults$percentile==75),"epoch"]] -
      presults[[which(presults$percentile==25),"epoch"]]
  }else {
    for (ptileName in percentileNames)
      df[1, ptileName] = NA
    
    for (ptileStrName in percentileStrNames)
      df[1, ptileStrName] = NA
    
    df[1, "IQR"] = NA
  }
  
  return (df)
}

computeTemporalMetrics <- function(channel, tableName, tableInfo, temporal_columns,
                                   percentiles, percentileNames, percentileStrNames, percentileStr, 
                                   where_clause, total_count, parallel=FALSE) {
  
  where_clause = ifelse(length(where_clause)>1, paste0(where_clause, " AND (%%%column__name%%%) IS NOT NULL"),
                        " WHERE (%%%column__name%%%) IS NOT NULL ")
  
  getTemporalMetricsSql <- function(name, type) {
    
    # this variable is not used - for reference only
    temporalTypes = c('timestamp', 'timestamp without time zone', 'timestamp with time zone','interval',
                      'date','time', 'time with timezone','time without time zone')
    
    sql = 
      paste0("SELECT cast(count(distinct (%%%column__name%%%)) as bigint) as distinct_count, ",
             "       cast(count((%%%column__name%%%)) as bigint) as not_null_count, ",
             "       min((%%%column__name%%%))::varchar as minimum, ",
             "       max((%%%column__name%%%))::varchar as maximum, ",
             ifelse(grepl("^(timestamp|date)", type, ignore.case=TRUE),
             "       to_timestamp(avg(extract('EPOCH' FROM (%%%column__name%%%)))) as average ",
             "       avg((%%%column__name%%%)) as average "),
             "  FROM ", tableName, where_clause) 
    
    gsub('(%%%column__name%%%)', name, sql, fixed=TRUE)
  }
  
  percentileSql = 
    paste0("SELECT percentile, MAX((%%%column__name%%%))::varchar value, 
                   MAX(EXTRACT('EPOCH' FROM (%%%column__name%%%))) epoch  
              FROM (SELECT (%%%column__name%%%), ntile(100) OVER (ORDER BY (%%%column__name%%%)) AS percentile
                      FROM ", tableName, where_clause, ") t
             WHERE percentile IN ( ", percentileStr, " ) 
             GROUP BY 1 ",
           # performance enhancement: include SELECT belo only when 0 percentile (min) requested
           ifelse(0 %in% percentiles, paste0(
          " UNION  
            SELECT 0, MIN((%%%column__name%%%)), MIN(EXTRACT('EPOCH' FROM (%%%column__name%%%))) epoch 
              FROM svplan.containertrailerplans ", where_clause),
          " "),
          "  ORDER BY 1")
  
  # non-functional: eliminates NOTE 'no visible binding for global variable'
  column_name = column_type = idx = NULL 
  
  if (!parallel) {
    result = foreach(column_name = tableInfo$COLUMN_NAME, column_type = tableInfo$TYPE_NAME,
                     idx = seq_along(tableInfo$COLUMN_NAME), 
                     .combine='rbind', .packages=c('RODBC')) %do% {
                       
                       if (column_name %in% temporal_columns) {
                         
                         # Compute SELECT aggregate statistics on each temporal column
                         column_stats = toaSqlQuery(channel, getTemporalMetricsSql(column_name, column_type), 
                                                    stringsAsFactors = FALSE)
                         
                         # compute all percentiles at once with SQL/MR approximate percentile function
                         presults = toaSqlQuery(channel, gsub('(%%%column__name%%%)', column_name, percentileSql, 
                                                              fixed=TRUE), stringsAsFactors = FALSE)
                         
                         makeTemporalMetrics(idx, column_stats, total_count, presults, percentileNames, percentileStrNames)
                       }
                     }
    
  }else{
    # parallel mode compute
    result = foreach(column_name = tableInfo$COLUMN_NAME, column_type = tableInfo$TYPE_NAME,
                     idx = seq_along(tableInfo$COLUMN_NAME),
                     .combine='rbind', .packages=c('RODBC'), .inorder=FALSE) %dopar% {
                       
                       if (column_name %in% temporal_columns) {
                         
                         parChan = odbcReConnect(channel)
                         # Compute SELECT aggregate statistics on each temporal column
                         column_stats = toaSqlQuery(parChan, getTemporalMetricsSql(column_name, column_type), 
                                                    stringsAsFactors = FALSE, closeOnError=TRUE)
                         
                         # compute all percentiles at once with SQL/MR approximate percentile function
                         presults = toaSqlQuery(parChan, gsub('(%%%column__name%%%)', column_name, percentileSql, 
                                                              fixed=TRUE), 
                                                stringsAsFactors = FALSE, closeOnError=TRUE)
                         close(parChan)
                         
                         makeTemporalMetrics(idx, column_stats, total_count, presults, percentileNames, percentileStrNames)
                       }  
                     }
  }
  
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
  
  # non-functional: eliminates NOTE 'no visible binding for global variable' 
  column_name = idx = NULL 
  
  if (!parallel) {
    result = foreach(column_name = tableInfo$COLUMN_NAME, idx = seq_along(tableInfo$COLUMN_NAME),
                     .combine='rbind', .packages=c('RODBC')) %do% {      
              
              mode = toaSqlQuery(channel, sub('(%%%column__name%%%)', column_name, sql, fixed=TRUE), 
                              stringsAsFactors = FALSE)
              
              makeMode(idx, mode)
             }
  }else {
    # parallel mode compute 
    result = foreach(column_name = tableInfo$COLUMN_NAME, idx = seq_along(tableInfo$COLUMN_NAME),
                     .combine='rbind', .packages=c('RODBC'), .inorder=FALSE) %dopar% {
              
              parChan = odbcReConnect(channel)
              mode = toaSqlQuery(parChan, sub('(%%%column__name%%%)', column_name, sql, fixed=TRUE), 
                              stringsAsFactors = FALSE, closeOnError=TRUE)
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
#' if(interactive()){
#' # initialize connection to Lahman baseball database in Aster 
#' conn = odbcDriverConnect(connection="driver={Aster ODBC Driver};
#'                          server=<dbhost>;port=2406;database=<dbname>;uid=<user>;pwd=<pw>")
#' 
#' pitchingInfo = getTableSummary(channel=conn, 'pitching_enh')
#' viewTableSummary(pitchingInfo, percentiles=TRUE)
#' 
#' viewTableSummary(pitchingInfo, types=c("numeric", "temporal"))
#' }
viewTableSummary <- function(tableInfo, types=NULL,
                             include=NULL, except=NULL, basic=FALSE, 
                             percentiles=FALSE) {
  
  if (!interactive()) return()
    
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
    
  utils::View(tableInfo[row_indices, col_indices])
  
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
    
    rs = toaSqlQuery(conn, sql, stringsAsFactors=FALSE)
    
    return (rs[, 'values'])
  }
}

#' Test if table present in the database.
#' 
#' @param channel object as returned by \code{\link{odbcConnect}}.
#' @param names vector of table names. Name may contain SQL wildcard 
#'   characters but it should not contain schema prefix. All visible schemas 
#'   will be checked for table name specified.
#' @return logical vector indicating if corresponding name is table in Aster database.
#' @seealso \code{\link{getTableSummary}}
#' @export
#' @examples 
#' if(interactive()){
#' # initialize connection to Lahman baseball database in Aster 
#' conn = odbcDriverConnect(connection="driver={Aster ODBC Driver};
#'                          server=<dbhost>;port=2406;database=<dbname>;uid=<user>;pwd=<pw>")
#' 
#' isTable(conn, "pitching")        # TRUE 
#' isTable(conn, "pitch%")          # TRUE
#' isTable(conn, "public.pitching") # FALSE
#' }
isTable <- function(channel, names) {
  if (is.null(names) || length(names) < 1) return(logical(0))
  
  vapply(names, FUN=function(x) nrow(sqlTables(channel, tableName=x))>0, FUN.VALUE=logical(1))
}

# TODO: make part of utility convinience set of functions
grantExecuteOnFunction <- function(conn, name='%', owner='db_superuser', user) {
  
  if (missing(user)) {
    stop("User name to grant execute permission is missing.")
  }
  
  sql = paste0("select * from nc_system.nc_all_sqlmr_funcs where funcname like '", name, "' and funcowner = '",
               owner, "'")
  
  func_list = toaSqlQuery(conn, sql)
  
  if (!('funcname' %in% names(func_list)) || length(func_list$funcname) == 0) {
    stop("No functions to grant execute permission to.")
  }
  
  for(func_name in func_list$funcname){
    sql = paste0("grant execute on function ", func_name, " to ", user)
    result = toaSqlQuery(conn, sql)
    
    return (result)
  }
}


#' Counts nulls per column in the table.
#' 
#' @param channel object as returned by \code{\link{odbcConnect}}.
#' @param tableName name of the table in Aster.
#' @param schema NULL or character: optional schema to restric table search to signle schema. In general,
#'   table search performed across whole database. Including \code{schema} restricts it to this single 
#'   schema only.
#' @param tableInfo pre-built summary of columns to use (require when \code{test=TRUE}). 
#'   See \code{\link{sqlColumns}} or \code{\link{getTableSummary}}.
#' @param include a vector of column names to include. Output never contains attributes other than in the list.
#' @param except a vector of column names to exclude. Output never contains attributes from the list.
#' @param where specifies criteria to satisfy by the table rows before applying computation. 
#'   The creteria are expressed in the form of SQL predicates (inside \code{WHERE} clause).
#' @param output Default output is a data frame in \code{'long'} format. Other options include 
#'   \code{'wide'} format and \code{'matrix'}.
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \link{RODBC} 
#'   functions like \link{sqlQuery} and \link{sqlSave}).
#' @export
#' @examples 
#' if (interactive()) {
#' # initialize connection to Dallas database in Aster 
#' conn = odbcDriverConnect(connection="driver={Aster ODBC Driver};
#'                          server=<dbhost>;port=2406;database=<dbname>;uid=<user>;pwd=<pw>")
#'
#' null_counts = getNullCounts(conn, "baseball.batting", 
#'                             include=c('g','ab','r','h','so','bb','cs'), 
#'                             where='yearid > 2000')
#' 
#' }
getNullCounts <- function(channel, tableName, tableInfo=NULL, include=NULL, except=NULL, 
                          schema=NULL, output='long', where=NULL, test=FALSE){
  
  # match argument values
  output = match.arg(output, c('long', 'wide', 'matrix'))
    
    ## table name is required
    if (missing(tableName)) {
      stop("Table name must be specified.")
    }
    
    tableName <- normalizeTableName(tableName)
    
    if (is.null(tableInfo) && test) {
      stop("Must provide tableInfo when test==TRUE.")
    }
    
    ## get column names
    if (is.null(tableInfo)) {
      tableInfo <- sqlColumns(channel, sqtable = tableName, schema = schema)
    }
    
    if (length(unique(tableInfo$TABLE_SCHEM)) > 1) 
      stop("Table name is not uqique - must provide schema using either parameter 'schema' or 'tableName'.")
    
    
    columnNames <- includeExcludeColumns(tableInfo, include, except)$COLUMN_NAME
    
    ## per column name, construct count nulls SQL
    columnNameNullSQL <- sapply(columnNames, constructCountNullsSQL)
    
    where_clause <- makeWhereClause(where)

    ## assemble full SQL query
    nullCountsSQL <- paste('SELECT',
                          paste(columnNameNullSQL, collapse = ', '),
                          'FROM', tableName, where_clause)

    ## execute SQL to collect counts
    if (test) 
      return(nullCountsSQL)
    else
      nullCountsWide <- sqlQuery(channel, nullCountsSQL)

    if (output == 'wide') 
      return (nullCountsWide)
    else if(output == 'long') {
      ## transpose results
      nullCountsLongMatrix <- t(nullCountsWide)
      nullCountsLong <- data.frame(variable = row.names(nullCountsLongMatrix),
                                   nullcount = as.integer(nullCountsLongMatrix))
      return (nullCountsLong)
    }else
      return (as.matrix(t(nullCountsWide)))
    
}

## construct count nulls string -- used in getNullCounts
constructCountNullsSQL <- function(variableName){
    sqlString <- paste0('COUNT(1) - COUNT(',variableName,') AS ', variableName)
    return(sqlString)
}