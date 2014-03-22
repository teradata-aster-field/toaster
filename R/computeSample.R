#' Randomly sample data from the table
#' 
#' Draws a sample of rows from the table randomly. The function offers two 
#' sampling schemes:
#'   - a simple binomial (Bernoulli) sampling on a row-by-row basis with
#'     given sample rate(s)
#'   - sampling a given number of rows without replacement
#' The sampling can be applied to the entire table or can be refined with 
#' conditions.
#' 
#' @param channel connection object as returned by \code{\link{odbcConnect}}
#' @param tableName table name
#' @param sampleFraction one or more sample fractions to use in the sampling of data. (multipe 
#'   sampling fractions are not yet supported.)
#' @param sampleSize total sample size (applies only when \code{sampleFraction} is missing).
#' @param include a vector of column names to include. Output never contains attributes other than in the list.
#' @param except a vector of column names to exclude. Output never contains attributes from the list.
#' @param where specifies criteria to satisfy by the table rows before applying
#'   computation. The creteria are expressed in the form of SQL predicates (inside
#'   \code{WHERE} clause).
#' @param stringsAsFactors logical: should character vectors returned as part of results be converted to factors? 
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \link{RODBC} 
#'   functions like \link{sqlQuery} and \link{sqlSave}).
#' 
#' @export
#' 
#' @examples
#' \donttest{
#' batters = computeSample(conn, "batting", sampleFraction=0.01)
#' dim(batters)
#'
#' pitchersAL = computeSample(conn, "pitching", sampleSize=1000,
#'                            where="lgid = 'AL'")
#' dim(ptichersAL)
#' }
computeSample <- function(channel, tableName, sampleFraction, sampleSize, 
                          include = NULL, except = NULL, 
                          where = NULL, stringsAsFactors = FALSE, 
                          test = FALSE) {
  
  if (missing(tableName)) {
    stop("Table name must be specified.")
  }
  
  if ((missing(sampleFraction) || is.null(sampleFraction)) && 
      (missing(sampleSize) || is.null(sampleSize))) {
    stop("Sample fraction or sample size must be specified.")
  }
  
  if (!missing(except) && missing(include)) {
    if (!test) 
      stop("Can't test without include but with except.")
    table_info = sqlColumns(channel, tableName)
    table_info = includeExcludeColumns(table_info, include, except)
    columns = table_info$COLUMN_NAMES
  }else if(!missing(include)){
    columns = setdiff(include, except)
  }else {
    columns = " * "
  }
  
  columnList = paste(columns, collapse = ", ")
  
  where_clause = makeWhereClause(where)
  
  if (!missing(sampleFraction) && !is.null(sampleFraction)) {
    stopifnot(sampleFraction >= 0, sampleFraction <= 1)
    
    # using fraction, ignore sample size
    sql = paste0("SELECT *   
                    FROM sample(
                           ON (SELECT ", columnList, " FROM ", tableName, where_clause, " )  
                           SampleFraction('", as.character(sampleFraction), "'))")
  }else {
    # using sample size
    sql = paste0("SELECT * 
                    FROM sample(
                           ON (SELECT ", columnList, " FROM ", tableName, where_clause, " ) 
                             AS DATA PARTITION BY ANY
                           ON (SELECT COUNT(*) as stratum_count FROM ", tableName, where_clause, ") 
                             AS SUMMARY DIMENSION
                           ApproximateSampleSize('", as.character(sampleSize), "'))")
  }
  
  if(test) {
    return(sql)
  }else {
    return(sqlQuery(channel, sql, stringsAsFactors=stringsAsFactors))
  }
  
}