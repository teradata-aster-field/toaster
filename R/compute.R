#' General function for computing data in Aster
#' 
#' This function performs SELECT and GROUP BY computation in Aster
#' standardizing access to Aster datasets.
#' 
#' 
compute <- function(channel, tableName, 
                    columnNames=vector(), 
                    aggregateFun="COUNT(*)", aggregateAlias = "cnt",
                    where=NULL) {
  
  where_clause = makeWhereClause(where)
  
  
}