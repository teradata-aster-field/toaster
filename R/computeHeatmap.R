require(RODBC)

#' computes data frame for heatmap visualizations
#' 
#' @param withMelt if TRUE (default FALSE) then will call \code{reshape2} \code{melt} with
#'                 parameters below
#' @param id.vars see \code{melt.data.frame}
#' @param measure.vars see \code{melt.data.frame}
#' @param variable.name see \code{melt.data.frame}
#' @param value.name see \code{melt.data.frame}
#' @param melt.na.rm see \code{melt.data.frame}
#' @export
computeHeatmap <- function(channel, tableName, dimension1, dimension2, 
                           aggregateFun="COUNT(*)", aggregateAlias = "cnt", percent=FALSE,
                           withMelt=FALSE, id.vars, measure.vars,
                           variable.name = "variable", value.name = "value", melt.na.rm = FALSE,
                           #TODO: implement withMelt functionality above
                           where=NULL, by=NULL) {
  
  where_clause = makeWhereClause(where)
  
  #validate aggregate args
  if (length(aggregateFun) != length(aggregateAlias)) 
    stop("Lengths of parameters 'aggregateFun' and 'aggregateAlias' must be the same")
  
  aggSelectList = paste(aggregateFun, aggregateAlias, sep=" ", collapse=", ")
  
  if (is.null(by)) {
    heatmap = sqlQuery(channel,
                       paste0("SELECT \"", dimension1, "\", \"", dimension2, "\", ", aggSelectList,
                              "  FROM ", tableName, 
                              where_clause,
                              " GROUP BY 1, 2")
    )
    
  }else {
    heatmap = sqlQuery(channel,
                       paste0("SELECT \"", by, "\", \"",  dimension1, "\", \"", dimension2, "\", ", aggSelectList,
                              "  FROM ", tableName, 
                              where_clause,
                              " GROUP BY 1, 2, 3")
    )
    
  }
  
  if (withMelt) {
    ## TODO: implement auto-assign for id.vars and measure.vars based on parameter values
    heatmap = melt(heatmap, id.vars=id.vars, meausre.vars=measure.vars, variable.name=variable.name, 
                   value.name=value.name, na.rm=melt.na.rm)
  }
  
  
  return (heatmap)
}