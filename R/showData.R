
#' Produces one of standard plots with statistics from the table.
#' 
#' @param channel
#' @param tableName
#' @param tableInfo pre-built summary of data to use (parameters \code{channel}, \code{tableName}, \code{where} will not apply)
#' @param include
#' @param except
#' @param numeric include numeric columns (default is TRUE)
#' @param datetime include date and time columns (default is TRUE)
#' @param character include character columns (default is TRUE)
#' @param format type of plot to use ('histogram', 'boxplot', or 'scatterplot')
#' @param facet Logical - if TRUE then divide plot into facets for each COLUMN (defualt is FALSE - no facets). 
#' When set to TRUE and format is 'boxplot' facetScales defalut changes from 'fixed' to free'.
#' @param ncol Number of columns in facet wrap
#' @param facetScales Are scales shared across all facets: "fixed" - all are the same, "free_x" - vary across rows (x axis),
#'        "free_y" - vary across columns (Y axis) (default), "free" - both rows and columns (see in \code{facet_wrap} 
#'        parameter \code{scales}. Also see parameter \code{facet} for details on default values. )
#' @return A ggplot visual
#' 
showData <- function(channel=NULL, tableName=NULL, tableInfo=NULL, include=NULL, except=NULL, where=NULL,
                     numeric=TRUE, datetime=TRUE, character=TRUE,
                     format=c('histogram','boxplot','scatterplot', 'corr'),
                     size=1000,
                     facet=FALSE, ncol=4, facetScales=ifelse(facet & format=='boxplot',"free", "fixed")) {
  
  if (missing(tableInfo)) {
    summary = getTableSummary(channel, tableName, include=include, except=except, where=where, collect.mode=FALSE)
  }else {
    summary = includeExcludeColumns(tableInfo, include, except)
  }
  
  if (format=='boxplot') {
    # reduce data set to numerical data types
    data = getNumericColumns(summary, names.only=FALSE)

    p = ggplot(data, aes(x = COLUMN_NAME)) +
      geom_boxplot(aes(ymin = `0%`, lower = `25%`, middle = `50%`, upper = `75%`, ymax = `100%`),
                   stat="identity", position="dodge") 
  }else if (format=='corr') {
    corrmat = computeCorrelations(channel, tableName, include=include, except=except, where=where)
    
    p = createBubblechart(corrmat, "metric1", "metric2", "value", label="corr", fill="sign")
  }
  
  if (facet) {
    p = p + facet_wrap(~COLUMN_NAME, ncol=ncol, scales=facetScales)
  }
  
  if (format=='histogram') {
    
    p = ggplot(summary, aes(x = COLUMN_NAME)) +
      geom_histogram(aes(y=distinct_count), stat="identity", position="dodge")
  }
  
  return(p)
}

