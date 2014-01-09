require(RODBC)

#' computes data frame for heatmap visualizations
#' 
#' @param withMelt logical if TRUE then uses \link{reshape2} \code{\link{melt}} to transform result data frame
#'  aggregate values into a molten data frame
#' @param id.vars see \code{melt.data.frame}
#' @param measure.vars see \code{melt.data.frame}
#' @param variable.name see \code{melt.data.frame}
#' @param value.name see \code{melt.data.frame}
#' @param melt.na.rm see \code{melt.data.frame}
#' @param where SQL WHERE clause limiting data from the table (use SQL as if in WHERE clause but omit keyword WHERE)
#' @param by for optional grouping by one or more values for faceting or alike
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \link{RODBC} 
#'   functions like \link{sqlQuery} and \link{sqlSave}).
#' @export
computeHeatmap <- function(channel, tableName, dimension1, dimension2, 
                           aggregateFun="COUNT(*)", aggregateAlias = "cnt", percent=FALSE,
                           withMelt=FALSE, id.vars, measure.vars,
                           variable.name = "variable", value.name = "value", melt.na.rm = FALSE,
                           #TODO: implement withMelt functionality above
                           where=NULL, by=NULL, test=FALSE) {
  
  where_clause = makeWhereClause(where)
  
  #validate aggregate args
  if (length(aggregateFun) != length(aggregateAlias)) 
    stop("Lengths of parameters 'aggregateFun' and 'aggregateAlias' must be the same")
  
  aggSelectList = paste(aggregateFun, aggregateAlias, sep=" ", collapse=", ")
  
  if (is.null(by)) {
     sql = paste0("SELECT \"", dimension1, "\", \"", dimension2, "\", ", aggSelectList,
                  "  FROM ", tableName, 
                  where_clause,
                  " GROUP BY 1, 2")
  }else {
     sql = paste0("SELECT \"", by, "\", \"",  dimension1, "\", \"", dimension2, "\", ", aggSelectList,
                  "  FROM ", tableName, 
                  where_clause,
                  " GROUP BY 1, 2, 3")  
  }
  
  if (test) {
    return (sql)
  }else {
    heatmap = sqlQuery(channel, sql)
  }
  
  if (withMelt) {
    ## TODO: implement auto-assign for id.vars and measure.vars based on parameter values
    heatmap = melt(heatmap, id.vars=id.vars, meausre.vars=measure.vars, variable.name=variable.name, 
                   value.name=value.name, na.rm=melt.na.rm)
  }
  
  
  return (heatmap)
}