#' Convinience methods for working with Aster data
#'

#' NOT IN Function
#' 
#' @export
`%notin%` <- function(x,y) !(x %in% y)


#' Augment Column table with Statistics on All Columns computed in Aster
#' 
#' Computes statistics for all table attributes (columns)
#' Value is based on RODBC sqlColumns table with additional columns
#' for computed statistics.
#' 
#' @param channel object as returned by \code{\link{odbcConnect}}
#' @param tableName database table name
#' @export
#' 
getTableSummary <- function (channel, tableName, include=NULL, except=NULL, where=NULL, collect.mode=FALSE) {
  table_info = sqlColumns(channel, tableName)
  
  table_info = includeExcludeColumns(table_info, include, except)
  
  # check if at least one column found
  if (nrow(table_info) == 0) stop(paste("No columns specified found in the table '", tableName, "'"))
  
  where_clause = makeWhereClause(where)
  
  # Create data frame columns for table statistics
  table_info[c("total_count","distinct_count","not_null_count")] = as.integer(NA)
  table_info[c("minimum","maximum","average","deviation",
               "0%","10%","25%","50%","75%","90%","100%","IQR")] = as.numeric(NA)
  table_info[c("minimum_str","maximum_str")] = as.character(NA)
  
  # Total rows
  total_rows = sqlQuery(channel,
                        paste0("SELECT COUNT(*) cnt FROM ", tableName, where_clause)
  )
  total_count = total_rows[[1,1]]
  table_info[, "total_count"] = total_count
  
  # Loop over numeric columns
  for(column_name in getNumericColumns(table_info)) {
    
    # Compute SELECT aggregate statistics on each numeric column
    column_stats = sqlQuery(channel,
                            paste0("SELECT count(distinct\"", column_name,"\") as distinct_count, ",
                                   "       count(\"", column_name, "\") as not_null_count, ",
                                   "       min(\"", column_name,"\") as minimum, ",
                                   "       max(\"", column_name,"\") as maximum, ",
                                   "       avg(cast(\"", column_name,"\" as bigint)) as average, ",
                                   "       stddev(cast(\"", column_name,"\" as bigint)) as deviation ",
                                   "  FROM ", tableName, where_clause)
    )
    column_idx = which(table_info$COLUMN_NAME == column_name)
    
    table_info[column_idx, "distinct_count"] = column_stats[[1,"distinct_count"]]
    table_info[column_idx, "not_null_count"] = column_stats[[1,"not_null_count"]]
    table_info[column_idx, "null_count"] = total_count - column_stats[[1,"not_null_count"]]
    table_info[column_idx, "minimum"] = column_stats[[1,"minimum"]]
    table_info[column_idx, "maximum"] = column_stats[[1,"maximum"]]
    table_info[column_idx, "average"] = column_stats[[1,"average"]]
    table_info[column_idx, "deviation"] = column_stats[[1,"deviation"]]
    
    # compute 0,25,50,75,100 percentiles with SQL/MR approximate percentile function
    percentiles = sqlQuery(channel,
                           paste0("SELECT * FROM approxPercentileReduce(
                                  ON (
                                  SELECT * FROM approxPercentileMap(
                                  ON  ( SELECT * FROM " , tableName, where_clause, " ) ",
                                  " TARGET_COLUMN( '",column_name,"' )
                                  ERROR( 1 )
                                  )
                                  )
                                  PARTITION BY 1
                                  PERCENTILE( 0,10,25,50,75,90,100 ))")
                           )
    
    table_info[column_idx, "0%"] = percentiles[[which(percentiles$percentile==0),"value"]]
    table_info[column_idx, "10%"] = percentiles[[which(percentiles$percentile==10),"value"]]
    table_info[column_idx, "25%"] = percentiles[[which(percentiles$percentile==25),"value"]]
    table_info[column_idx, "50%"] = percentiles[[which(percentiles$percentile==50),"value"]]
    table_info[column_idx, "75%"] = percentiles[[which(percentiles$percentile==75),"value"]]
    table_info[column_idx, "90%"] = percentiles[[which(percentiles$percentile==90),"value"]]
    table_info[column_idx, "100%"] = percentiles[[which(percentiles$percentile==100),"value"]]
    table_info[column_idx, "IQR"] = percentiles[[which(percentiles$percentile==75),"value"]] -
      percentiles[[which(percentiles$percentile==25),"value"]]
  }
  
  # Loop over non-numeric columns 
  non_numeric_columns = c(getCharacterColumns(table_info),
                          getDateTimeColumns(table_info))
  for(column_name in non_numeric_columns) {
    column_stats = sqlQuery(channel,
                            paste0("SELECT count(distinct\"", column_name, "\") as distinct_count, ",
                                   "       count(\"", column_name, "\") as not_null_count, ",
                                   "       min(\"", column_name, "\") as minimum, ",
                                   "       max(\"", column_name, "\") as maximum ",
                                   "  FROM ", tableName, where_clause)
    )
    column_idx = which(table_info$COLUMN_NAME == column_name)
    table_info[column_idx, "distinct_count"] = column_stats[[1,"distinct_count"]]
    table_info[column_idx, "not_null_count"] = column_stats[[1,"not_null_count"]]
    table_info[column_idx, "null_count"] = total_count - column_stats[[1,"not_null_count"]]
    table_info[column_idx, "minimum_str"] = column_stats[[1,"minimum"]]
    table_info[column_idx, "maximum_str"] = column_stats[[1,"maximum"]]
  }
  
  # Collect modes
  if(collect.mode) {
    for(column_name in table_info$COLUMN_NAME) {
      mode = sqlQuery(channel,
                      paste0("SELECT \"", column_name, "\" val, count(*) cnt ",
                             "  FROM ", tableName, where_clause,
                             " GROUP BY 1 ORDER BY 2 DESC LIMIT 1")
      )
      column_idx = which(table_info$COLUMN_NAME == column_name)
      table_info[column_idx, "mode"] = toString(mode[[1,"val"]])
      table_info[column_idx, "mode_count"] = mode[[1, "cnt"]]
    }
    
  }
  
  
  return(table_info)
  
}

#' Test if table has lookup column in another table
#'
#' @export
isLookupForColumn  <- function(channel, tableName, columnName, lookupTable, lookupColumn=NULL,
                               ignoreCase=FALSE) {
  
  # ignore case or not
  if (ignoreCase) {
    columnName = paste0("lower(", columnName, ")")
    lookupColumn = paste0("lower(", lookupColumn, ")")
  }
  
  # check total number of distinct values in the data set
  result = sqlQuery(channel, 
                    paste0("SELECT COUNT(DISTINCT ", columnName, ") cnt  
                           FROM ", tableName)
                    )
  totalValues = result$cnt[[1]]
  
  # check number of distinct values in the data set that have lookups in lookup table 
  result = sqlQuery(channel,
                    paste0("SELECT COUNT(DISTINCT ", columnName, ") cnt 
                              FROM ", tableName, 
                           " WHERE ", columnName, " IN (SELECT ", lookupColumn, " FROM ", lookupTable, ")")
  )
  resolvedValues = result$cnt[[1]]
  
  result = sqlQuery(channel, 
                    paste0("SELECT COUNT(DISTINCT ", lookupColumn, ") cnt_dist, 
                                   COUNT(", lookupColumn, ") cnt 
                              FROM ", lookupTable)
  )
  lookupValues = result$cnt_dist[[1]]
  lookupTotalValues = result$cnt[[1]]
  
  # Report results
  print(paste0("Total values in ", tableName, ".", columnName, ": ", totalValues))
  print(paste0("Total values found in ", lookupTable, ".", lookupColumn, ": ", resolvedValues))
  print(paste0("Lookup table support is ", sprintf("%.2f",(resolvedValues/totalValues)*100.), "%"))
  if (lookupTotalValues > lookupValues) {
    print(paste0("WARNING: ", lookupTable, ".", lookupColumn, " contains ", 
                 lookupTotalValues - lookupValues, " duplicate values"))
  }
  
  return()
  
}

#' view important Table statistics in spreadsheet-style viewer 
#' 
#' @export
viewTableSummary <- function(tableInfo, columns=NULL, basic=FALSE, percentiles=FALSE) {
  if (missing(tableInfo)) return()
  
  col_indices = c("COLUMN_NAME")
  
  if(basic) {
    col_indices = c(col_indices, "minimum", "maximum", "average", "deviation")
  }
  
  if(percentiles) {
    col_indices = c(col_indices, "0%", "10%", "25%", "50%", "75%", "100%", "IQR")
  }
  
  if (length(col_indices) == 1) {
    col_indices = c(4,19:ncol(tableInfo))
  }
  
  if(missing(columns)) {
    row_indices = 1:nrow(tableInfo)
  }else {
    row_indices = tableInfo$COLUMN_NAME %in% columns
  }
  
  View(tableInfo[row_indices, col_indices])
  
  return(1.0)
}