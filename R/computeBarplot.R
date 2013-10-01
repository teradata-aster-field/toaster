require(RODBC)

#' Compute Barplot
#' 
#' \code{computeBarplot} is convinient wrapper for assembling barplot
#' data from Aster tables.
#' 
#' @param columnNames list of columns with optional aliases (may include expressions)
#' @export computeBarplot
#' 
#' 
computeBarplot <- function(channel, tableName, columnNames=vector(),
                           aggregateFun="COUNT(*)", aggregateAlias = "cnt", percent=FALSE,
                           where=NULL, by=NULL) {
  
  where_clause = makeWhereClause(where)
  
  # construct column list
  columnList = paste(columnNames, collapse=", ")
  # construct group by list by removing aliases (if any)
  groupByList = paste(sub(columnNames, pattern = " [a-zA-Z0-9_]*$", replacement = ""), collapse=", ")
  selectColumnList = paste(paste(columnList, aggregateFun, sep=", "), aggregateAlias)
  bars = sqlQuery(asterConn,
                  paste0("SELECT ", selectColumnList, " FROM ", tableName,  
                         where_clause,
                         " GROUP BY ", groupByList)
  )
  
  return(bars)
}