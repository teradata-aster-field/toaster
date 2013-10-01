#' Compute correlations between all or specified numeric columns in a table
#'
#' @param channel object as returned by \code{\link{odbcConnect}}
#' @param tableName database table name
#' @param include a vector of column names to include. Output is restricted to this list.
#' @param except a vector of column names to exclude. Output never contains names from this list.
#' 
#' @export
computeCorrelations <- function(channel, tableName, include=NULL, except=NULL, where=NULL) {
  table_info = sqlColumns(channel, tableName)
  
  columns = getNumericColumns(table_info, names.only=TRUE, include=include, except=except)
  
  correlations = subset(expand.grid(columns, columns, stringsAsFactors = FALSE),
                        subset=Var1<Var2)
  correlations = apply(correlations, 1, function(x) paste(x, collapse=':'))
  
  sqlmr_correlations = paste(correlations, collapse="', '")
  sql_corr_columns = paste(columns, collapse="\", \"")
  
  where_clause = makeWhereClause(where)
  
  rs_corrs = sqlQuery(channel, 
                      paste0("SELECT * FROM corr_reduce(
                  ON corr_map(
                    ON ( SELECT \"", sql_corr_columns, "\" FROM ", tableName, where_clause, 
                    " )
                    columnpairs( '", sqlmr_correlations, "')
                    key_name('key')
                  )
                  partition by key
                )"))
  
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
  
  return(rs_corrs)
}