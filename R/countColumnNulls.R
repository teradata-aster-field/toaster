## function to count nulls per variable in dataset
getNullCounts <- function(channel, tableName, include=NULL, except=NULL, where=NULL, test=FALSE){
    require(RODBC)

    ## get column names
    columnNames <- sqlColumns(ac, sqtable = tableName)$COLUMN_NAME

    ## per variable, collect number of null values
    nullCounts <- list()
    for (v in variableNames) {
        nullCounts[[v]] <- sqlQuery(channel,
                                    count_nulls_sql_query(v, tableName))
    }

    ## rbind all null count data frames together
    nullCountsDF <- do.call('rbind', nullCounts)

    ## order by missingness, descending
    nullCountsDF <- nullCountsDF[with(nullCountsDF, order(is_null, cnt, decreasing = TRUE)),]
    return(nullCountsDF)
}


## function to construct null count SQL query
count_nulls_sql_query <- function(variableName, tableName){
    qry <- paste0("select
'", variableName, "' as variable_name,
(case when ", variableName, " is null then 1 else 0 end) as is_null,
count(1) as cnt
from
", tableName,
" group by (case when ", variableName, " is null then 1 else 0 end);")
    return(qry)
}


## test functions
library(RODBC)
ac <- odbcConnect('aster20.7W')
channel <- ac
columnNames <- sqlColumns(ac, sqtable = 'baseball.appearances')$COLUMN_NAME

sqlQuery(ac, count_nulls_sql_query('var1', 'tbl1', 'schema1'))

nullCounts <- count_column_nulls(ac, 'tbl1', 'schema1')
nullCounts
