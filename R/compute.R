#' Compute aggregates on a table.
#' 
#' Computes aggregates by means of \code{SELECT ... GROUP BY} computation on Aster
#' table.
#' 
#' @param channel object as returned by \code{\link{odbcConnect}}
#' @param tableName table name
#' @param by column names and expressions to group aggregates (with SQL \code{GROUP BY ...}). It may be 
#'   table column names, valid SQL expressions and can contain otional alias (e.g. \code{"UPPER(car_make) make"})
#' @param aggregates SQL aggregates to compute. Aggregates may have optional aliases like in \code{"AVG(era) avg_era"}
#' @param stringsAsFactors logical: should columns returned as character and not excluded by as. is and not converted to 
#'   anything else be converted to factors?
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \link{RODBC} 
#'   functions like \link{sqlQuery} and \link{sqlSave}).
#'   
#' @examples
#' \donttest{
#' data = compute(asterConn, "teams_enh",
#'                by = c("name || ', ' || park teamname", "lgid", "teamid", "decadeid"),
#'                aggregates = c("min(name) name", "min(park) park", "avg(rank) rank", "avg(attendance) attendance")
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
  
  if (missing(by) | length(by) == 0) {
    stop("Must have one or more columns/expressions in 'by' parameter.")
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