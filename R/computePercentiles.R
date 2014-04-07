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
#' @param columnName name of the column to compute percentiles on
#' @param percentiles integer vector with percentiles to compute. Values \code{0, 25, 50, 75, 100}
#'    will always be added if omitted.
#' @param by for optional grouping by one or more values for faceting or alike. 
#'   If used with \code{\link{createBoxplot}} then use first name for x-axis and 
#'   the rest for wrap or grid faceting.
#' @param where specifies criteria to satisfy by the table rows before applying
#'   computation. The creteria are expressed in the form of SQL predicates (inside
#'   \code{WHERE} clause).
#' @param stringsAsFactors logical: should columns returned as character and not excluded by \code{as.is}
#'   and not converted to anything else be converted to factors?
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \link{RODBC} 
#'   functions like \link{sqlQuery} and \link{sqlSave}).
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
  
  if (missing(channel)) {
    stop("Must provide connection.")
  }
  
  
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