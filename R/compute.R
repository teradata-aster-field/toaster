#' Compute aggregate values
#' 
#' Compute aggregates using SQL \code{SELECT...GROUP BY} in Aster. Aggregates may be any 
#' valid SQL expressions (including SQL \code{WINDOW} functions) in context of group 
#' columns (parameter \code{by}). Neither SQL \code{ORDER BY} nor \code{LIMIT} clauses
#' are supported (use \code{\link{computeBarchart}} when they are required).  
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
#'                               "avg(attendance) attendance"))
#'                
#' # compute total strike-outs for each team in decades starting with 1980
#' # and also percent (share) of team strikeouts within a decade
#' data = compute(channel = conn, "pitching_enh",
#'                by = c("teamid", "decadeid"), 
#'                aggregates = c("sum(so) so", 
#'                               "sum(so)/(sum(sum(so)) over (partition by decadeid)) percent"),
#'                where = "decadeid >= 1980")
#' }
#'   
#' @export
#' 
compute <- function(channel, tableName, 
                    aggregates = "COUNT(*) cnt", 
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