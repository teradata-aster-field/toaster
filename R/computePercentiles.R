#' Compute percentiles
#' 
#' Compute percentiles including boxplot quartiles.
#'   
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
computePercentiles <- function(channel, tableName, columnName,
                               percentiles = c(0,5,10,25,50,75,90,95,100),
                               by = NULL, where = NULL, 
                               stringsAsFactors = FALSE, test = FALSE) {
  
  
  if (missing(tableName) || missing(columnName) || 
        is.null(tableName) || is.null(columnName)) {
    stop("Must provide table and column names.")
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
  
  sql = paste0("SELECT * FROM approxPercentileReduce(
                                  ON (
                                    SELECT * FROM approxPercentileMap(
                                      ON  ( SELECT * FROM " , tableName, where_clause, " ) ",
               "                      TARGET_COLUMN( '",columnName,"' )
                                      ERROR( 1 ) ",
                                      groupColumnsOpt,
               "                     )
                                     )
                                  PARTITION BY ", partitionByList, 
               "                  PERCENTILE( ", percentileStr, " )",
                                  groupColumnsOpt,
               "                  )")
  
  if (test) {
    return(sql)
  }else {
    return(sqlQuery(channel, sql, stringsAsFactors=stringsAsFactors))
  }
  
}