#' Compute table aggregates.
#' 
#' Compute aggregates defined with SQL aggregate syntax and aliases utilizing
#' SQL \code{SELECT...GROUP BY} in Aster. Any SQL expressions that are valid 
#' aggregates supported, including window function \code{OVER}. 
#' 
#' @param channel connection object as returned by \code{\link{odbcConnect}}
#' @param tableName table name
#' @param by character vecotr of column names and/or expressions on which grouping is performed 
#'   (with SQL \code{GROUP BY ...}). Each can be a column or a valid SQL non-aggregate expression    
#'   with otional alias separated by space (e.g. \code{"UPPER(car_make) make"}).
#' @param aggregates SQL aggregates to compute. Aggregates may have optional aliases like in \code{"AVG(era) avg_era"}
#' @param where SQL WHERE clause limiting data from the table (use SQL as if in WHERE clause but omit keyword WHERE) 
#' @param stringsAsFactors logical: should character vectors returned as part of results be converted to factors? 
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \link{RODBC} 
#'   functions like \link{sqlQuery} and \link{sqlSave}).
#' @aliases computeAggregates  
#' @examples
#' \donttest{
#' data = compute(channel = conn, tableName = "teams_enh",
#'                by = c("name || ', ' || park teamname", "lgid", "teamid", "decadeid"),
#'                aggregates = c("min(name) name", "min(park) park", "avg(rank) rank", 
#'                               "avg(attendance) attendance")
#'                )
#' }
#'   
#' @export
#' 
compute <- function(channel, tableName, 
                    aggregates = c("COUNT(*) cnt"), 
                    by = vector(), where = NULL, 
                    stringsAsFactors = FALSE, test = FALSE) {
  
  if (missing(tableName)) {
    stop("Must have table name.")
  }
  
  if (missing(by) || length(by) == 0) {
    stop("Must have one or more columns/expressions in 'by' parameter.")
  }
  
  if (is.null(aggregates) || length(aggregates) < 1) {
    stop("Must have at least one aggregate defined.")
  }
  
  where_clause = makeWhereClause(where)
  
  columnExpr = sub(by, pattern = " [a-zA-Z0-9_]*$", replacement = "")
  
  # construct column list
  columnList = paste(paste(by, collapse=", "), paste(aggregates, collapse=", "), sep=", ")
  # construct group by list by removing aliases (if any)
  groupByList = paste(columnExpr, collapse=", ")
  # construct sql
  sql = paste0("SELECT ", columnList, " FROM ", tableName,  
               where_clause,
               " GROUP BY ", groupByList)
  
  if (test) {
    return(sql)
  }else {
    return(sqlQuery(channel, sql, stringsAsFactors=stringsAsFactors))
  }
  
}