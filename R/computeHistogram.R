#' Compute column histogram
#' 
#' @param channel object as returned by \code{\link{odbcConnect}}
#' @param tableName database table name
#' @param columnName table column name to compute histogram
#' @param columnFrequency logical indicates to build histogram of frequencies of column
#' @param datepart field to extract from timestamp/date/time column to build histogram on
#' @param useIQR logical indicates if to use IQR-range (Q1-1.5*IQR, Q3+1.5*IQR) to limit values profiled (TRUE by default)
#' 
#' @export
computeHistogram <- function(channel, tableName, columnName, columnFrequency=FALSE,
                             query=NULL,
                             binsize=NULL, startvalue=NULL, endvalue=NULL, numbins=NULL,
                             useIQR=TRUE,
                             where=NULL, by=NULL, datepart=NULL) {
  
  where_clause = makeWhereClause(where)
  
  if (columnFrequency) {
    return (computeHistogramOfFrequencies(channel, tableName, columnName, 
                                                binsize, startvalue, endvalue, numbins,
                                                where_clause, by))
  }
  
  column_stats = getTableSummary(channel, tableName, include=columnName, where=where)
  
  # check if histogram is for character column
  # if so use SQL GROUP BY
  if (isCharacterColumn(column_stats, columnName)) {
    return (computeSQLHistogram(channel, tableName, columnName, 
                                where_clause, by))
  }
  
  # set number of bins default if NULL
  if (is.null(numbins)) {
    numbins = 30
  }
  
  # check if histogram is for date/time column
  # if so use EXTRACT function and SQL/MR
  if (isDateTimeColumn(column_stats, columnName)) {
    return (computeDateHistogram(channel, tableName, columnName, 
                                 binsize, startvalue, endvalue, numbins,
                                 useIQR,
                                 where_clause, by, datepart))
  }
  
  # compute histogram parameters if missing
  if (is.null(binsize) | is.null(startvalue) | is.null(endvalue)) {
    
    IQR = column_stats[[1,"IQR"]]
    MIN = column_stats[[1,"minimum"]]
    MAX = column_stats[[1,"maximum"]]
    Q1 = column_stats[[1,"25%"]]
    Q3 = column_stats[[1,"75%"]]
    
    if (is.null(startvalue)) {
      if (useIQR) {
        startvalue = max(MIN, Q1-1.5*IQR)
      }else {
        startvalue = MIN
      }
    }
    
    if (is.null(endvalue)) {
      if (useIQR) {
        endvalue = min(MAX, Q3+1.5*IQR)
      }else {
        endvalue = MAX
      }
    }
    
    if (is.null(binsize)) {
      binsize = (endvalue - startvalue) / numbins
    }
    
  }
  
  # No by clause - single histogram
  if (is.null(by)) {
    histogram = sqlQuery(channel,
                       paste0("SELECT * 
                                 FROM hist_reduce(
                                        ON hist_map(
                                          ON (SELECT cast(\"", columnName, "\" as numeric) ", columnName, " FROM ", tableName, where_clause,
                                          "  ) 
                                          binsize('", binsize, "')
                                          startvalue('", startvalue, "')
                                          endvalue('", endvalue, "')
                                          value_column('", columnName, "')
                                        ) 
                                        partition by 1
                                      )")
                         
    )
  # By clause - multiple histograms for each value of 'by' attribute
  }else {
    histogram = sqlQuery(channel,
                         paste0("SELECT * 
                                 FROM hist_reduce(
                                        ON hist_map(
                                          ON (SELECT \"", by, "\", cast(\"", columnName, "\" as numeric) ", columnName, " FROM ", tableName, where_clause,
                                "  ) 
                                          binsize('", binsize, "')
                                          startvalue('", startvalue, "')
                                          endvalue('", endvalue, "')
                                          value_column('\"", columnName, "\"')
                                          by('\"", by, "\"')
                                        ) 
                                        partition by \"", by, "\" 
                                      )")
                         
    )
  }
  
  return (histogram)
}

computeHistogramOfFrequencies <- function(channel, tableName, columnName, 
                                                binsize, startvalue, endvalue, numbins,
                                                where_clause, by) {
  
  if (is.null(by)) {
    histogram = sqlQuery(channel,
                         paste0("SELECT * 
                                 FROM hist_reduce(
                                        ON hist_map(
                                          ON (SELECT \"", columnName, "\", count(*) cnt 
                                                FROM ", tableName, where_clause,
                                              " GROUP BY 1 
                                            )
                                          binsize('", binsize, "')
                                          startvalue('", startvalue, "')
                                          endvalue('", endvalue, "')
                                          value_column('cnt')                                          
                                        ) 
                                        partition by 1 
                                      )")
    )
  }else {
    histogram = sqlQuery(channel,
                         paste0("SELECT * 
                                 FROM hist_reduce(
                                        ON hist_map(                                          
                                          ON (SELECT \"", by, "\", \"", columnName, "\", count(*) cnt 
                                                FROM ", tableName, where_clause,
                                "               GROUP BY 1, 2  
                                            )
                                          binsize('", binsize, "')
                                          startvalue('", startvalue, "')
                                          endvalue('", endvalue, "')
                                          value_column('cnt')
                                          by('\"", by, "\"')
                                        ) 
                                        partition by \"", by, "\" 
                                      )")
    )
  }
  
  return (histogram)
  
}

computeSQLHistogram <- function(channel, tableName, columnName, 
                                where_clause, by=NULL) {
  
  if (is.null(by)) {
    histogram = sqlQuery(channel,
                         paste0("SELECT \"", columnName, "\", COUNT(*) cnt 
                                   FROM ", tableName, where_clause,
                                "GROUP BY 1 
                                 ORDER BY 2 DESC ")
                         )
  }else {
    histogram = sqlQuery(channel,
                         paste0("SELECT \"", by, "\" by, \"", columnName, "\", COUNT(*) cnt
                                   FROM ", tableName, where_clause,
                                "GROUP BY 1, 2
                                 ORDER BY 1 ASC, 3 DESC"))
  }
  
  return (histogram)
}

computeDateHistogram <- function(channel, tableName, columnName, 
                                 binsize=NULL, startvalue=NULL, endvalue=NULL, numbins=NULL,
                                 useIQR=NULL,
                                 where_clause, by=NULL, datepart='DAY') {
  
  if (is.null(binsize) | is.null(startvalue) | is.null(endvalue)) {
    
    # compute percentiles first
    percentiles = sqlQuery(channel,
                           paste0("SELECT * FROM approxPercentileReduce(
                                    ON (
                                      SELECT * FROM approxPercentileMap(
                                      ON  ( SELECT EXTRACT('", datepart, "' FROM \"", columnName, "\" ) dp FROM " , tableName, where_clause, " ) ",
                                  "   TARGET_COLUMN( 'dp' )
                                      ERROR( 1 )
                                      )
                                    )
                                    PARTITION BY 1
                                    PERCENTILE( 0,25,50,75,100 ))")                     
    )
    MIN = percentiles[[which(percentiles$percentile==0),"value"]]
    MAX = percentiles[[which(percentiles$percentile==100),"value"]]
    Q1 = percentiles[[which(percentiles$percentile==25),"value"]]
    Q3 = percentiles[[which(percentiles$percentile==75),"value"]]
    IQR = Q3 - Q1
    
    if (is.null(startvalue)) {
      if (useIQR) {
        startvalue = max(MIN, Q1-1.5*IQR)
      }else {
        startvalue = MIN
      }
    }
    if (is.null(endvalue)) {
      if (useIQR) {
        endvalue = min(MAX, Q3+1.5*IQR)
      }else {
        endvalue = MAX
      }
    }
    if (is.null(binsize)) { 
      binsize = (endvalue - startvalue) / numbins
    }
  }
  
  # No by clause - single histogram
  if (is.null(by)) {
    histogram = sqlQuery(channel,
                         paste0("SELECT * 
                                 FROM hist_reduce(
                                        ON hist_map(
                                          ON (SELECT EXTRACT('", datepart, "' FROM \"", columnName, "\" ) dp FROM ", tableName, where_clause,
                                "  ) 
                                          binsize('", binsize, "')
                                          startvalue('", startvalue, "')
                                          endvalue('", endvalue, "')
                                          value_column('dp')
                                        ) 
                                        partition by 1
                                      )")
                         
    )
  # By clause - multiple histograms for each value of 'by' attribute
  }
  
  return (histogram)
}