require(RODBC)

#' Compute Barchart for a category and arbitrary number of aggregates.
#' 
#' Computes data suitable for bar chart plotting with single category corresponding to a column or 
#' an expression. Function can compute arbitrary number of aggregates.
#' 
#' @param channel object as returned by \code{\link{odbcConnect}}
#' @param tableName table name
#' @param category column name or expression corresponding to bar chart category. Name may be 
#'   valid SQL expression and can contain otional alias (e.g. \code{"UPPER(car_make) make"})
#' @param aggregates SQL aggregates to compute. Aggregates may have optional aliases like in \code{"AVG(era) avg_era"}
#' @param by for optional grouping by one or more columns for faceting or alike (effectively these elements
#'   will be part of \code{GROUP BY ...})
#' @param percent logical: 
#' @param where SQL WHERE clause limiting data from the table (use SQL as if in WHERE clause but omit keyword WHERE)
#' @param orderBy list of column names, aliases, references or their combinations to use in SQL \code{ORDER BY} 
#' clause
#' @param top if specified indicates number of bars to include in bar plot. In combination with \code{orderBy} 
#'   it works as computing first \code{top} results.
#' @param withMelt logical if TRUE then uses \pkg{reshape2} \code{\link{melt}} to transform result data frame
#'  aggregate values into a molten data frame
#' @param stringsAsFactors logical: should columns returned as character and not excluded by as. is and not converted to 
#'   anything else be converted to factors?
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \link{RODBC} 
#'   functions like \link{sqlQuery} and \link{sqlSave}).
#' 
#' @export
#' @seealso computeHistogram
#' 
#' @examples
#' \donttest{
#' # Compute average team season era, walks, and hits for each decaded starting with 1980
#' computeBarchart(asterConn, "teams_enh", "teamid team", c("avg(era) era", "avg(bb) bb", "avg(h) h"), 
#'                 where="yearid >=1980", by=c("decadeid"))
#' }
computeBarchart <- function(channel, tableName, category,
                            aggregates = "COUNT(*) cnt", percent = FALSE,
                            where = NULL, orderBy = NULL, top = NULL, by = NULL, 
                            withMelt = FALSE,
                            stringsAsFactors = FALSE, test = FALSE) {
  
  if (missing(tableName)) {
    stop("Must have table name.")
  }
  
  if (missing(category) | length(category) != 1) {
    stop("Bar chart must have exactly one category.")
  }
  
  where_clause = makeWhereClause(where)
  
  orderby_clause = makeOrderByClause(orderBy)
  
  limit_clause = makeLimitClause(top)
  
  categoryExpr = sub(category, pattern = " [a-zA-Z0-9_]*$", replacement = "")
  
  if (missing(by)) {
    # construct column list
    columnList = paste(category, paste(aggregates, collapse=", "), sep=", ")
    # construct group by list by removing aliases (if any)
    groupByList = paste(categoryExpr, collapse=", ")
  }else {
    # construct column list
    columnList = paste(category, paste(by, collapse=", "), paste(aggregates, collapse=", "), sep=", ")
    # construct group by list by removing aliases (if any)
    groupByList = paste(categoryExpr, paste(by, collapse=", "), sep=", ")
  }
  
  sql = paste0("SELECT ", columnList, " FROM ", tableName,  
               where_clause,
               " GROUP BY ", groupByList,
               orderby_clause, 
               limit_clause)
  
  if (test) {
    return(sql)
  }else {
    df = sqlQuery(channel, sql, stringsAsFactors=stringsAsFactors)
    
    if (withMelt) {
      df = melt(df, id.vars=c(category, by))
    }
    
    return(df)
  }

}