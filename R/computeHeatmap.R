require(RODBC)

#' computes data frame for heatmap visualizations
#' 
#' @param channel connection object as returned by \code{\link{odbcConnect}}
#' @param tableName table name
#' @param dimension1 name of the column for for heatmap x values. This value along with \code{dimension2}
#'   are x and y scales of heatmap table.
#' @param dimension2 name of the column for for heatmap y values. This value along with \code{dimension1}
#'   are x and y scales of heatmap table.
#' @param aggregateFun vector with aggregate functions to compute and use for for heatmap values. In SQL it translates 
#'   to \code{GROUP BY dimension1, dimension2} and computes one or more aggregates. 
#' @param aggregateAlias vector of aliases for aggregate values in SQL. This becomes names of value columns in 
#'   heatmap data frame. Subsequently, \code{createHeatmap} will use these values to assign color (fill), text,
#'   and threshold values to heatmap cells. 
#' @param withMelt logical if TRUE then uses \pkg{reshape2} \code{\link{melt}} to transform result data frame
#'  aggregate values into a molten data frame
#' @param id.vars see \code{melt.data.frame}
#' @param measure.vars see \code{melt.data.frame}
#' @param variable.name see \code{melt.data.frame}
#' @param value.name see \code{melt.data.frame}
#' @param melt.na.rm see \code{melt.data.frame}
#' @param where SQL WHERE clause limiting data from the table (use SQL as if in WHERE clause but omit keyword WHERE)
#' @param by for optional grouping by one or more values for faceting or alike
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \pkg{RODBC} 
#'   functions: \link{sqlQuery} and \link{sqlSave}).
#' @export
#' @seealso createHeatmap
#' 
#' @examples
#' \donttest{
#' hm = computeHeatmap(asterConn, "teams_enh", 'franchid', 'decadeid', 'avg(w)', 'w', where="decadeid >= 1950")
#' hm$decadeid = factor(hm$decadeid)
#' createHeatmap(hm, 'decadeid', 'franchid', 'w')
#' 
#' # with diverging color gradient
#' hm = computeHeatmap(asterConn, "teams_enh", 'franchid', 'decadeid', 'avg(w-l)', 'wl', where="decadeid >= 1950")
#' hm$decadeid = factor(hm$decadeid)
#' createHeatmap(hm, 'decadeid', 'franchid', 'wl', divergingColourGradient = TRUE)
#' }
computeHeatmap <- function(channel, tableName, dimension1, dimension2, 
                           aggregateFun="COUNT(*)", aggregateAlias = "cnt", 
                           withMelt=FALSE, id.vars, measure.vars,
                           variable.name = "variable", value.name = "value", melt.na.rm = FALSE,
                           #TODO: implement withMelt functionality above
                           where=NULL, by=NULL, test=FALSE) {
  
  if (missing(tableName)) {
    stop("Must have table name.")
  }
  
  if (missing(dimension1) || missing(dimension2)) {
    stop("Must have all 2 heatmap dimensions defined to compute.")
  }
  
  where_clause = makeWhereClause(where)
  
  #validate aggregate args
  if (length(aggregateFun) != length(aggregateAlias)) 
    stop("Lengths of parameters 'aggregateFun' and 'aggregateAlias' must be the same.")
  
  aggSelectList = paste(aggregateFun, aggregateAlias, sep=" ", collapse=", ")
  
  if (is.null(by)) {
     sql = paste0("SELECT ", dimension1, ", ", dimension2, ", ", aggSelectList,
                  "  FROM ", tableName, 
                  where_clause,
                  " GROUP BY 1, 2")
  }else {
     sql = paste0("SELECT ", by, ", ",  dimension1, ", ", dimension2, ", ", aggSelectList,
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