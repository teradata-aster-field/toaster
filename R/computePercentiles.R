#' Compute percentiles of column values.
#' 
#' Compute percentiles including boxplot quartiles across values of column 
#' \code{columnName}. Multiple sets of percentiles achieved with the
#' parameter \code{by}. Vector \code{by} may contain arbitrary number 
#' of column names: the percentiles are computed for each combination
#' of values from these columns. Remember that when using computed
#' quartiles with function \code{\link{createBoxplot}} it can utilize
#' up to 3 columns by displaying them along the x-axis and inside
#' facets.
#'   
#' @param channel connection object as returned by \code{\link{odbcConnect}}
#' @param tableName Aster table name
#' @param columnName deprecated. Use \code{columnNames} instead. 
#' @param columnNames names of the columns to compute percentiles on
#' @param percentiles integer vector with percentiles to compute. Values \code{0, 25, 50, 75, 100}
#'    will always be added if omitted.
#' @param by for optional grouping by one or more values for faceting or alike. 
#'   Used with \code{\link{createBoxplot}} in combination with column name for x-axis and 
#'   wrap or grid faceting.
#' @param parallel logical: enable parallel calls to Aster database. This option requires parallel 
#'   backend enabled and registered (see in examples). Parallel execution requires ODBC \code{channel} 
#'   obtained without explicit password: either with \code{\link{odbcConnect}(dsn)} or 
#'   \code{\link{odbcDriverConnect}} calls, but not with \code{\link{odbcConnect}(dsn, user, password)}.
#' @param where specifies criteria to satisfy by the table rows before applying
#'   computation. The creteria are expressed in the form of SQL predicates (inside
#'   \code{WHERE} clause).
#' @param nameInDataFrame name of the column in returned data frame to store table column name(s)  
#'   defined by parameter \code{columnNames}. \code{NULL} indicates omit this column from the data 
#'   frame (not recommended when computing percentiles for two or more columns).
#' @param stringsAsFactors logical: should columns returned as character and not excluded by \code{as.is}
#'   and not converted to anything else be converted to factors?
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \link{RODBC} 
#'   functions like \link{sqlQuery} and \link{sqlSave}).
#' @return Data frame containing percentile values organized into following columns:
#'   \itemize{
#'     \item \emph{percentile} percentile to compute (from 0 to 100): will contain all valid values 
#'       from \code{percentiles}
#'     \item \emph{value} computed percentile
#'     \item \emph{column} table column name. Override name \code{column} with parameter \code{nameInDataFrame}
#'       or omit this column all together if \code{NULL}.
#'     \item \emph{by[1], by[2], ...} in presence of parameter \code{by}, contain values of the grouping 
#'       columns for computed percentiles (optional). 
#'   }
#' 
#' @export
#' @examples
#' \donttest{
#' 
#' # ipouts percentiles for pitching ipouts for AL in 2000s
#' ipop = computePercentiles(conn, "pitching", "ipouts",
#'                           where = "lgid = 'AL' and yearid >= 2000")
#' 
#' # ipouts percentiles by league
#' ipopLg = computePercentiles(conn, "pitching", "ipouts", by="lgid")
#' 
#' }
computePercentiles <- function(channel, tableName, columnName = NULL, columnNames = columnName,
                               percentiles = c(0,5,10,25,50,75,90,95,100),
                               by = NULL, where = NULL, nameInDataFrame = 'column',
                               stringsAsFactors = FALSE, test = FALSE, parallel = FALSE) {
  
  if (!is.null(columnName)) {
    toa_dep("0.2.5", "\"columnName\" argument in computePercentiles is deprecated. Use columnNames for columns to compute percentiles on.")
  }
  
  if (missing(channel)) {
    stop("Must provide connection.")
  } 
  
  if (missing(tableName) || is.null(tableName))
    stop("Must provide table name.")
    
  if ((missing(columnName) && missing(columnNames)) ||
        is.null(columnNames) ||
        length(columnNames) == 0) {
    stop("Must provide at least one column name.")
  }
  
  # percentiles
  # always add 50 (median) and 25, 75 for IQR computations
  if (is.null(percentiles) | !is.numeric(percentiles)) {
    percentiles = c(0, 25, 50, 75, 100)
  }else {
    percentiles = union(percentiles, c(0, 25, 50, 75, 100)) 
  }
  if (any(percentiles < 0 | percentiles > 100)) {
    stop (paste("Invalid percentile value(s) passed (below 0 or above 100): ", percentiles))
  }
  percentiles = sort(percentiles)
  
  where_clause = makeWhereClause(where)
  
  percentileNames = paste0(percentiles, "%")
  names(percentileNames) = percentiles
  percentileStr = paste(percentiles, collapse=",")
  
  if (missing(by)) {
    # construct column list
    partitionByList = " 1 " 
    # construct group by list by removing aliases (if any)
    groupColumnsOpt = " "
  }else {
    # construct column list
    partitionByList = paste(by, collapse=", ")
    # construct group by list by removing aliases (if any)
    groupColumnsOpt = paste(" GROUP_COLUMNS(", paste0("'", by, "'", collapse=", "), ")", sep=" ")
  }
  
  
  if (test) {
    sql = assemblePercentileSql(tableName, where_clause, columnNames[[1]], partitionByList, percentileStr, groupColumnsOpt)
    return(sql)
  }else {
    if (!parallel) {
      result = foreach(name = columnNames, .combine='rbind', .packages=c('RODBC')) %do% {
        sql = assemblePercentileSql(tableName, where_clause, name, partitionByList, percentileStr, groupColumnsOpt)
        rs = sqlQuery(channel, sql, stringsAsFactors=stringsAsFactors)
        if (!is.null(nameInDataFrame))
          rs[, nameInDataFrame] = name
        rs
      }
    }else {
      result = foreach(name = columnNames, .combine='rbind', .packages=c('RODBC')) %dopar% {
        sql = assemblePercentileSql(tableName, where_clause, name, partitionByList, percentileStr, groupColumnsOpt)
        parChan = odbcReConnect(channel)
        rs = sqlQuery(parChan, sql, stringsAsFactors=stringsAsFactors)
        close(parChan)
        if (!is.null(nameInDataFrame))
          rs[, nameInDataFrame] = name
        rs
      }
    }
    
    return(result)
  }
  
}

assemblePercentileSql <- function(tableName, where_clause, name, partitionByList, percentileStr, groupColumnsOpt) {
  
  sql = paste0("SELECT * FROM approxPercentileReduce(
                                  ON (
                                    SELECT * FROM approxPercentileMap(
                                      ON  ( SELECT * FROM " , tableName, where_clause, " ) ",
               "                      TARGET_COLUMN( '", name, "' )
                                      ERROR( 1 ) ",
               groupColumnsOpt,
               "                     )
                                     )
                                  PARTITION BY ", partitionByList, 
               "                  PERCENTILE( ", percentileStr, " )",
               groupColumnsOpt,
               "                  )")
}