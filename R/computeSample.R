#' Randomly sample data from the table.
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
#' @param sampleFraction one or more sample fractions to use in the sampling of data. Multipe 
#'   sampling fractions are applicable only in combination with the arguments \code{conditionColumn} 
#'   and \code{conditionValues} when present. In this case number of fractions in \code{sampleFraction} 
#'   and number of values in \code{conditionValues} must be the same.
#' @param sampleSize total sample size (applies only when \code{sampleFraction} is missing). This 
#'   may too be a vector of total values when used in combination with the arguments \code{conditionColumn} and
#'   \code{conditionValues}. In this case number of sizes in \code{sampleSize} and number of values in \code{conditionValues}
#'   must be the same.
#' @param conditionColumn if you use this argument, you must also use the \code{conditionValues} argument. 
#'   Either both are used, or neither is used. Values in a particular column \code{conditionColumn}
#'   are used as sampling conditions directly and its data type must be of a group-able type. Only those values
#'   listed in \code{conditionValues} are used for sampling with the rest ignored. Also, see \code{conditionStratum}.
#' @param conditionStratum if you use this argument, you must also use the \code{conditionValues} argument. When defined
#'   it is used in place of \code{conditionColumn}. \code{conditionStratum} should define a SQL expression 
#'   (usually using \code{CASE} function but not necessarily). Resulting sample data frame will contain a column named \code{stratum} 
#'   just as if \code{conditionColumn = 'stratum'} was used. Arguments \code{conditionColumn} and \code{conditionStratum}
#'   are mutually exclusive: the former is ignored if both are defined.
#' @param conditionValues see argument \code{conditionColumn} and \code{conditionStratum}.
#' @param include a vector of column names to include. Output never contains attributes other than in the list.
#' @param except a vector of column names to exclude. Output never contains attributes from the list.
#' @param where specifies criteria to satisfy by the table rows before applying
#'   computation. The creteria are expressed in the form of SQL predicates (inside
#'   \code{WHERE} clause).
#' @param as.is which (if any) columns returned as character should be converted to another type? 
#'   Allowed values are as for \code{\link{read.table}}. See also \code{\link{sqlQuery}}.
#' @param stringsAsFactors logical: should columns returned as character and not excluded by \code{as.is}
#'   and not converted to anything else be converted to factors? 
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \link{RODBC} 
#'   functions like \link{sqlQuery} and \link{sqlSave}).
#' 
#' @export
#' @examples
#' if(interactive()){
#' # initialize connection to Lahman baseball database in Aster 
#' conn = odbcDriverConnect(connection="driver={Aster ODBC Driver};
#'                          server=<dbhost>;port=2406;database=<dbname>;uid=<user>;pwd=<pw>")
#' 
#' batters = computeSample(conn, "batting", sampleFraction=0.01)
#' dim(batters)
#'
#' pitchersAL = computeSample(conn, "pitching", sampleSize=1000, where="lgid = 'AL'")
#' dim(ptichersAL)
#' 
#' battersByDecadesSingleSize = computeSample(conn, tableName = 'batting_enh', 
#'                                            sampleSize=1000, 
#'                                            conditionColumn = 'decadeid', 
#'                                            conditionValues = c(1990,2000,2010))
#' dim(battersByDecadesSingleSize)
#' 
#' battersByDecades = computeSample(conn, tableName = 'batting_enh',
#'                                  sampleFraction = c(0.01,0.01,0.02), 
#'                                  conditionColumn = 'decadeid', conditionValues = c(1990,2000,2010))
#' dim(battersByDecades)
#' 
#' battersByOddEvenYears = computeSample(channel=NULL, tableName = 'batting_enh',
#'                                       sampleFraction = c(0.01,0.02),
#'                                       include = c('decadeid','g','ab','r','h'),
#'                                       conditionStratum = "yearid % 2", 
#'                                       conditionValues = c('0','1'),
#'                                       where = "lgid = 'NL'")
#' dim(battersByOddEvenYears)
#' 
#' battersBeforeAfter1960 = computeSample(channel=NULL, tableName = 'batting_enh',
#'                                        sampleSize = c(200, 200), 
#'                                        conditionStratum = "CASE WHEN yearid <- 1960 THEN 'before'
#'                                                                 ELSE 'after'
#'                                                           END", 
#'                                        conditionValues = c('before','after'))
#' dim(battersBeforeAfter1960)
#' }
computeSample <- function(channel, tableName, sampleFraction, sampleSize, conditionColumn = NULL,
                          conditionStratum = NULL, conditionValues = NULL, include = NULL, except = NULL, 
                          where = NULL, as.is = FALSE, stringsAsFactors = FALSE, 
                          test = FALSE) {
  
  if (missing(tableName)) {
    stop("Table name must be specified.")
  }
  
  if ((missing(sampleFraction) || is.null(sampleFraction)) && 
      (missing(sampleSize) || is.null(sampleSize))) {
    stop("Sample fraction or sample size must be specified.")
  }
  
  if(!is.null(conditionStratum))
    stratumColumn = paste(conditionStratum, 'as stratum')
  
  if (!missing(except) && missing(include)) {
    if (!test) 
      stop("Can't test without include but with except.")
    table_info = sqlColumns(channel, tableName)
    table_info = includeExcludeColumns(table_info, union(include, conditionColumn), except)
    columns = table_info$COLUMN_NAMES
  }else if(!missing(include)){
    columns = union(setdiff(include, except), conditionColumn)
    if(!is.null(conditionStratum))
      columns = c(columns, stratumColumn)
  }else {
    columns = " * "
    if(!is.null(conditionStratum))
      columns = c(columns, stratumColumn)
  }
  
  if (!missing(sampleFraction) && !is.null(sampleFraction))
    stopifnot(sampleFraction >= 0, sampleFraction <= 1)
  
  # validate conditions if present
  
  if (!is.null(conditionColumn) && !is.null(conditionStratum)) 
    stop("Both condition column and strata can't be defined. Use either one or another.")
  
  if (!is.null(conditionStratum))
    conditionColumn = 'stratum'
  
  if (xor(is.null(conditionColumn), is.null(conditionValues)))
      stop("Both condition column and condition values must be either present or NULLs.")
  
  if (!is.null(conditionColumn)) {
    
    if (!missing(sampleFraction) && length(sampleFraction) > 1 && length(sampleFraction) != length(conditionValues)) 
      stop("Number of fractions must match the number of condition values.")
    
    if (!missing(sampleSize) && length(sampleSize) > 1 && length(sampleSize) != length(conditionValues))
      stop("Number of sample sizes must match the number of condition values.")
    
  } 
  
  isValidConnection(channel, test)
  
  columnList = paste(columns, collapse = ", ")
  
  where_clause = makeWhereClause(where)
  
  # condition arguments if present
  if(!is.null(conditionColumn)) {
    
    conditionArgs = paste0(
      " ConditionOnColumn('", conditionColumn, "')
        ConditionOn(", makeSqlMrValueList(conditionValues), ")"
    )
    
    stratumSelectSql = paste0(
      "SELECT ", ifelse(is.null(conditionStratum), conditionColumn, conditionStratum), " as stratum, 
               COUNT(*) as stratum_count 
         FROM ", tableName, where_clause, " GROUP BY 1")
  }else {
    conditionArgs = ""
    
    stratumSelectSql = paste0("SELECT COUNT(*) as stratum_count FROM ", tableName, where_clause)
  }
  
  if (!missing(sampleFraction) && !is.null(sampleFraction)) {
    # using fraction, ignore sample size
    sql = paste0("SELECT *   
                    FROM sample(
                           ON (SELECT ", columnList, " FROM ", tableName, where_clause, " )  
                           SampleFraction(", makeSqlMrValueList(as.character(sampleFraction)), ")",
                           conditionArgs,")")
  }else {
    # using sample size
    sql = paste0("SELECT * 
                    FROM sample(
                           ON (SELECT ", columnList, " FROM ", tableName, where_clause, " ) 
                             AS DATA PARTITION BY ANY
                           ON (",stratumSelectSql,") 
                             AS SUMMARY DIMENSION
                           ApproximateSampleSize(", makeSqlMrValueList(format(sampleSize, scientific=FALSE)), ")",
                           conditionArgs,")")
  }
  
  if(test) {
    return(sql)
  }else {
    return(toaSqlQuery(channel, sql, as.is=as.is, stringsAsFactors=stringsAsFactors))
  }
  
}