#' Compute correlations between all or specified numeric columns in a table
#'
#' @param channel connection object as returned by \code{\link{odbcConnect}}
#' @param tableName database table name
#' @param tableInfo pre-built summary of data to use (must have with \code{test=TRUE})
#' @param include a vector of column names to include. Output never contains attributes other than in the list.
#' @param except a vector of column names to exclude. Output never contains attributes from the list.
#' @param where SQL WHERE clause limiting data from the table (use SQL as if in WHERE clause but omit keyword WHERE)
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \link{RODBC} 
#'   functions like \link{sqlQuery} and \link{sqlSave}).
#' @export
computeCorrelations <- function(channel, tableName, tableInfo, include, except=NULL, where=NULL, test=FALSE) {
  
  if (test & missing(tableInfo)) {
    stop("Must provide tableInfo when test==TRUE.")
  }
  
  if (missing(tableInfo)) {
    tableInfo = sqlColumns(channel, tableName)
  }
  
  columns = getNumericColumns(tableInfo, names.only=TRUE, include=include, except=except)
  
  correlations = expand.grid(columns, columns, stringsAsFactors = FALSE)
  correlations = with(correlations, correlations[Var1<Var2,])
  correlations = apply(correlations, 1, function(x) paste(x, collapse=':'))
  
  sqlmr_correlations = paste(correlations, collapse="', '")
  sql_corr_columns = paste(columns, collapse=", ")
  
  where_clause = makeWhereClause(where)
  
   
  sql = paste0("SELECT * FROM corr_reduce(
                  ON corr_map(
                    ON ( SELECT ", sql_corr_columns, " FROM ", tableName, where_clause, 
                    " )
                    columnpairs( '", sqlmr_correlations, "')
                    key_name('key')
                  )
                  partition by key
                )")
  
  if (test) {
    return (sql)
  }else {
    rs_corrs = sqlQuery(channel, sql)
  }
  
  rs_corrs = cbind(rs_corrs, t(sapply(rs_corrs$corr, 
                                      FUN=function(v) unlist(strsplit(toString(v), split=":")))))
  colnames(rs_corrs)[3] = 'metric1'
  colnames(rs_corrs)[4] = 'metric2'
  
  # make data frame symmetrical
  temp = rs_corrs
  temp[,c('metric1','metric2')] = rs_corrs[,c('metric2','metric1')]
  rs_corrs = rbind(rs_corrs, temp)
  
  # produce sign column
  signs = ifelse(sign(rs_corrs$value)>0, "1", ifelse(sign(rs_corrs$value)<0, "-1", "0"))
  rs_corrs$sign = factor(signs, levels=c("-1","0","1"), ordered=TRUE)
  
  # make metric columns ordered factors with the same levels (for sorting in plots)
  rs_corrs$metric1 = factor(rs_corrs$metric1, levels=unique(rs_corrs$metric1), ordered=TRUE)
  rs_corrs$metric2 = factor(rs_corrs$metric2, levels=unique(rs_corrs$metric1), ordered=TRUE)
  
  return(rs_corrs)
}