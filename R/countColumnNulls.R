## function to count nulls per variable in dataset
getNullCounts <- function(channel, tableName, include=NULL, except=NULL, where=NULL, test=FALSE){
    require(RODBC)

    ## get column names
    columnNames <- sqlColumns(channel, sqtable = tableName)$COLUMN_NAME

    ## per column name, construct count nulls SQL
    columnNameNullSQL <- sapply(columnNames, constructCountNullsSQL)

    ## assemble full SQL query
    nullCountsSQL <- paste('select',
                          paste(columnNameNullSQL, collapse = ', '),
                          'from', tableName)

    ## execute SQL to collect counts
    nullCountsWide <- sqlQuery(channel, nullCountsSQL)

    ## transpose results
    nullCountsLongMatrix <- t(nullCountsWide)
    nullCountsLong <- data.frame(variable = row.names(nullCountsLongMatrix),
                                 nullcount = as.integer(nullCountsLongMatrix))

    return(nullCountsLong)
}

## construct count nulls string
constructCountNullsSQL <- function(variableName){
    sqlString <- paste0('count(1) - count(',variableName,') as ', variableName)
    return(sqlString)
}


## test functions
library(RODBC)
channel <- odbcConnect('aster20.7W')

system.time(
    nc <- getNullCounts(channel, tableName = 'baseball.appearances')
    )

print(nc)
