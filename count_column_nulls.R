## function to count nulls per variable in dataset
count_column_nulls <- function(conn, tableName, schemaName){
    require(RODBC)

    ## get variable names
    variableNames <- names(sqlQuery(conn, paste0('select * from ', schemaName,'.',tableName,' limit 0;')))

    ## per variable, collect number of null values
    nullCounts <- list()
    for (v in variableNames) {
        nullCounts[[v]] <- sqlQuery(conn,
                                    count_nulls_sql_query(v, tableName, schemaName))
    }

    ## rbind all null count data frames together
    nullCountsDF <- do.call('rbind', nullCounts)

    ## order by missingness, descending
    nullCountsDF <- nullCountsDF[with(nullCountsDF, order(is_null, cnt, decreasing = TRUE)),]
    return(nullCountsDF)
}


## function to construct null count SQL query
count_nulls_sql_query <- function(variableName, tableName, schemaName){
    qry <- paste0("select
'", variableName, "' as variable_name,
(case when ", variableName, " is null then 1 else 0 end) as is_null,
count(1) as cnt
from
", schemaName, ".", tableName,
" group by (case when ", variableName, " is null then 1 else 0 end);")
    return(qry)
}


## test functions
library(RODBC)
ac <- odbcConnect('dsn', pwd = 'mypassword')

sqlQuery(ac, count_nulls_sql_query('var1', 'tbl1', 'schema1'))

nullCounts <- count_column_nulls(ac, 'tbl1', 'schema1')
nullCounts
