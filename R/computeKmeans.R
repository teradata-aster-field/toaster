#' Perform k-means clustering on the table.
#' 
#' @param channel connection object as returned by \code{\link{odbcConnect}}.
#' @param tableName Aster table name.
#' @param tableInfo pre-built summary of data to use (require when \code{test=TRUE}). See \code{\link{getTableSummary}}.
#' @param id column name or SQL expression containing unique table key.
#' @param idAlias SQL alias for table id. This is required when SQL expression is given for \code{id}.
#' @param include a vector of column names with variables (must be numeric). Model never contains variables other than in the list.
#' @param except a vector of column names to exclude from variables. Model never contains variables from the list.
#' @param centers either the number of clusters, say \code{k}, or a matrix of initial (distinct) cluster centres. 
#'   If a number, a random set of (distinct) rows in x is chosen as the initial centres. If a matrix then number 
#'   of rows determines the number of clusters as each row determines initial center.
#' @param aggregates vector with SQL aggregates to compute for each cluster identified by this function. Aggregate may have 
#'   optional aliases like in \code{"AVG(era) avg_era"}. Subsequently, use in \code{createClusterPlot} as cluster properties.
#' @param scale logical if TRUE then scale each variable in-database before clustering. Currently scaling to 0 mean and unit
#'   standard deviation only is supported.
#' @param where specifies criteria to satisfy by the table rows before applying
#'   computation. The creteria are expressed in the form of SQL predicates (inside
#'   \code{WHERE} clause).
#' @param scaledTableName name of Aster table with results of scaling
#' @param centroidTableName name of Aster table with centroids found by kmeans  
#' @param schema name of Aster schema tables \code{scaledTableName} and \code{centroidTableName} belong.
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \pkg{RODBC} 
#'   functions: \link{sqlQuery} and \link{sqlSave}).
#' 
#' @export
#' @examples 
#' if(interactive()){
#' # initialize connection to Lahman baseball database in Aster 
#' conn = odbcDriverConnect(connection="driver={Aster ODBC Driver};
#'                          server=<dbhost>;port=2406;database=<dbname>;uid=<user>;pwd=<pw>")
#'                          
#' km = computeKmeans(conn, "batting", 
#'                    aggregates = c("COUNT(*) cnt", "AVG(g) avg_g", "AVG(r) avg_r", "AVG(h) avg_h"),
#'                    id="playerid || '-' || stint || '-' || teamid || '-' || yearid", 
#'                    include=c('g','r','h'), scaledTableName='kmeans_test_scaled', 
#'                    centroidTableName='kmeans_test_centroids', schema='baseball',
#'                    where="yearid > 2000", test=FALSE)
#' kms = computeClusterSample(conn, km, 0.01, test=FALSE)
#' createClusterPairsPlot(kms)
#' }
computeKmeans <- function(channel, tableName, centers, iterMax=10, 
                          tableInfo, id, include=NULL, except=NULL, 
                          aggregates="COUNT(*) cnt", scale=TRUE, idAlias="id", 
                          where=NULL, scaledTableName=NULL, centroidTableName=NULL, schema=NULL,
                          test=FALSE) {
  
  if (test & missing(tableInfo)) {
    stop("Must provide tableInfo when test==TRUE")
  }
  
  if (!is.numeric(centers)) {
    stop("Parameter centers must be numeric.")
  }
  
  tableName = normalizeTableName(tableName)
  
  if (missing(tableInfo)) {
    tableInfo = sqlColumns(channel, tableName)
  }
  
  columns = getNumericColumns(tableInfo, names.only=TRUE, include=include, except=except)
  columns = setdiff(columns, id)
  
  if (is.null(columns) || length(columns) < 1) {
    stop("Kmeans operates on one or more numeric variables.")
  }
  
  if(idAlias %in% columns)
    stop(paste0("Id alias '", idAlias, "' can't be one of variable names."))
  
  if (is.null(scaledTableName))
    scaledTableName = makeTempTableName('scaled', 30, schema)
  else if (!is.null(schema))
    scaledTableName = paste0(schema, ".", scaledTableName)
  
  if(is.null(centroidTableName))
    centroidTableName = makeTempTableName('centroids', 30, schema)
  else if (!is.null(schema))
    centroidTableName = paste0(schema, ".", centroidTableName)
  
  where_clause = makeWhereClause(where)
  
  if(test)
    sqlSave = ""
  else
    sqlSave = NULL
  
  # scale data
  sqlDrop = paste("DROP TABLE IF EXISTS", scaledTableName)
  sql = getDataPrepSql(tableName, scaledTableName, columns, id, idAlias, where_clause)
  if(test) {
    sqlSave = sqlDrop
    sqlSave = paste(sqlSave, sql, sep=';\n')
  }else {
    toaSqlQuery(channel, sqlDrop)
    toaSqlQuery(channel, sql)
  }
    
  
  # run kmeans
  sqlDrop = paste("DROP TABLE IF EXISTS", centroidTableName)
  sql = getKmeansSql(scaledTableName, centroidTableName, centers, iterMax)
  if(test) {
    sqlSave = paste(sqlSave, sqlDrop, sep=';\n')
    sqlSave = paste(sqlSave, sql, sep=';\n')
  }else {
    toaSqlQuery(channel, sqlDrop)
    kmeansResultStr = toaSqlQuery(channel, sql, stringsAsFactors=FALSE)
    if (kmeansResultStr[1,'message'] == "Successful!" &&
        kmeansResultStr[2,'message'] == "Algorithm converged.") {
      iter = as.integer(gsub("[^0-9]", "", ts[3,'message']))
    }else {
      msg = paste(kmeansResultStr[,'message'], collapse="\n")
      stop(msg)
    }
  }
  
  
  # compute cluster stats
  sql = getKmeansStatsSql(tableName, scaledTableName, centroidTableName, columns, 
                         id, idAlias, aggregates, where_clause)
  if(test)
    sqlSave = paste0(paste(sqlSave, sql, sep=';\n'), ';')
  else
    data = toaSqlQuery(channel, sql, stringsAsFactors=FALSE)
  
  # return sql
  if(test) {
    return(sqlSave)
  }
  
  result = makeKmeansResult(data, centers, iter, tableName, columns, scaledTableName, centroidTableName, id, idAlias, where_clause)
  
  return(result)
}


# Phase 1: Data Prep
getDataPrepSql <- function(tableName, tempTableName, columns, id, idAlias, whereClause) {
  
  sqlmr_column_list = makeSqlMrColumnList(columns)
  query_as_table = getDataSql(tableName, columns, id, idAlias, whereClause)
  
  scaleMapSql = paste0(
    "SELECT * FROM ScaleMap (
       ON (", query_as_table, ")
    InputColumns (", sqlmr_column_list, ")
    -- MissValue ('OMIT')
    )"
  )
  
  scaleSql = paste0(
    "SELECT * FROM Scale(
       ON (", query_as_table, ") AS input PARTITION BY ANY
       ON (", scaleMapSql, ") AS STATISTIC DIMENSION
       Method ('STD')
       Accumulate('", idAlias, "')
       GlobalScale ('false')
       InputColumns (", sqlmr_column_list, ")
     )"
  )
  
  tempTableSql = paste0(
    "CREATE FACT TABLE ", tempTableName, " DISTRIBUTE BY HASH(", idAlias, ") AS 
       ", scaleSql
  )
}

# Phase: kmeans 
getKmeansSql <- function(scaledTableName, centroidTableName, centers, maxiternum, threshold='0.0395') {
  
  if (is.matrix(centers)) {
    initCenters = paste0("MEANS(", paste0("'", paste0(apply(centers, 1, paste0, collapse='_'), collapse="', '"), "'"), ")")
  }else
    initCenters = paste0("NUMBERK('", centers, "')")
  
  kmeansSql = paste0(
    "SELECT * FROM kmeans(
      ON (SELECT 1)
      PARTITION BY 1
      INPUTTABLE('", scaledTableName, "')
      OUTPUTTABLE('", centroidTableName, "')
   ", initCenters, "
      THRESHOLD('", threshold, "')
      MAXITERNUM('", maxiternum, "')
    )")
}

getKmeansStatsSql <- function(tableName, scaledTableName, centroidTableName, columns, id, idAlias, aggregates, whereClause) {
  
  clustersWithValuesSql = paste0(
    "SELECT clusterid, means, ", paste(aggregates, collapse=", "), 
    "  FROM (", getClusteredDataSql(tableName, scaledTableName, centroidTableName, columns, id, idAlias, whereClause), "
   ) clustered_data
     GROUP BY clusterid, means")
  
  
}

getDataSql <- function(tableName, columns, id, idAlias, whereClause) {
  
  paste0("SELECT ", id, " ", idAlias, ", ", makeSqlColumnList(columns), " FROM ", tableName, whereClause)
}


getClusteredDataSql <- function(tableName, scaledTableName, centroidTableName, columns, id, idAlias, whereClause) {
  
  query_as_table = getDataSql(tableName, columns, id, idAlias, whereClause)
  
  paste0(
    "SELECT c.clusterid, c.means, d.* 
      FROM ", centroidTableName, " c JOIN 
    kmeansplot (
      ON ", scaledTableName, " PARTITION BY ANY
      ON ", centroidTableName, " DIMENSION
      centroidsTable('",centroidTableName,"')
    ) kmp ON (c.clusterid = kmp.clusterid) JOIN 
    (", query_as_table, ") d on (kmp.", idAlias, " = d.", idAlias, ")"
  )
}


makeKmeansResult <- function(data, centers, iter, tableName, columns, scaledTableName, centroidTableName, id, idAlias, whereClause) {
  
  # parse data (kmeansplot) and form kmeans object
  centroids = data.frame(factor(data$clusterid),
                         t(matrix(unlist(strsplit(as.vector(data$means), split = " ")), ncol=length(data$means), nrow=length(columns))),
                         stringsAsFactors = FALSE)
  names(centroids) = c("clusterid", columns)
  centroids[,columns] = apply(centroids[,columns], 2, function(x) as.numeric(x))
  
  aggregates = data.frame(clusterid=centroids$clusterid, data[, c(-1,-2)])
  
  z <- structure(list(centroids=centroids,
                      aggregates=aggregates,
                      centers=centers,
                      iter=iter,
                      tableName=tableName,
                      columns=columns,
                      scaledTableName=scaledTableName,
                      centroidTableName=centroidTableName,
                      id=id,
                      idAlias=idAlias,
                      whereClause=whereClause
  ),
  class = c("toakmeans"))
  
  z
}


#' Random sample of clustered data
#' 
#' @param channel connection object as returned by \code{\link{odbcConnect}}.
#' @param km result of k-means clustering obtained with \code{computeKmeans}.
#' @param sampleFraction one or more sample fractions to use in the sampling of data. (multipe 
#'   sampling fractions are not yet supported.)
#' @param sampleSize total sample size (applies only when \code{sampleFraction} is missing).
#' @param scaled logical indicates if function uses original (default) or scaled variables.
#' @param includeId logical indicates if sample should include the key uniquely identifying
#'   each data row.
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \pkg{RODBC} 
#'   functions: \link{sqlQuery} and \link{sqlSave}).
#' 
#' @export
#' @examples 
#' if(interactive()){
#' # initialize connection to Lahman baseball database in Aster 
#' conn = odbcDriverConnect(connection="driver={Aster ODBC Driver};
#'                          server=<dbhost>;port=2406;database=<dbname>;uid=<user>;pwd=<pw>")
#'                          
#' km = computeKmeans(conn, "batting", 
#'                    aggregates = c("COUNT(*) cnt", "AVG(g) avg_g", "AVG(r) avg_r", "AVG(h) avg_h"),
#'                    id="playerid || '-' || stint || '-' || teamid || '-' || yearid", 
#'                    include=c('g','r','h'), scaledTableName='kmeans_test_scaled', 
#'                    centroidTableName='kmeans_test_centroids', schema='baseball',
#'                    where="yearid > 2000", test=FALSE)
#' kms = computeClusterSample(conn, km, 0.01, test=FALSE)
#' createClusterPairsPlot(kms)
#' }
computeClusterSample <- function(channel, km, sampleFraction, sampleSize, scaled=FALSE, includeId=FALSE, test=FALSE) {
  
  if (missing(km)) {
    stop("Kmeans object must be specified.")
  }
  
  if ((missing(sampleFraction) || is.null(sampleFraction)) && 
      (missing(sampleSize) || is.null(sampleSize))) {
    stop("Sample fraction or sample size must be specified.")
  }
  
  table_name = km$tableName
  columns = km$columns
  scaled_table_name = km$scaledTableName
  centroid_table_name = km$centroidTableName
  id = km$id
  idAlias = km$idAlias
  where_clause = km$whereClause
  centers = km$centers
  
  conditionOnSql = paste0("'",paste0(1:centers-1, collapse="','"),"'")
  query_as_table = getDataSql(table_name, columns, id, idAlias, where_clause)
  
  
  if (!missing(sampleFraction) && !is.null(sampleFraction)) {
    
    # using sample fraction
    stopifnot(sampleFraction >= 0, sampleFraction <= 1)
    
    sql = paste0(
      "SELECT * FROM sample(
             ON (SELECT ", ifelse(scaled,  " d.* ", " clusterid, d.* "), 
      "            FROM kmeansplot(
                     ON ", scaled_table_name, " PARTITION BY ANY
                     ON ", centroid_table_name, " DIMENSION
                     centroidsTable('",centroid_table_name,"')
                   ) ", ifelse(scaled, " d ", 
                               paste0( " kmp JOIN (", query_as_table, ") d ON (kmp.", idAlias, " = d.", idAlias, ")")), "
                  WHERE clusterid != -1
             )
             CONDITIONONCOLUMN('clusterid')
             CONDITIONON(",conditionOnSql,")
             SAMPLEFRACTION('", as.character(sampleFraction), "')
       )")
  }else {
    # using sample size
    sql = paste0(
      "WITH stratum_counts AS (
         SELECT clusterid stratum, count(*) stratum_count 
           FROM kmeansplot(
             ON ", scaled_table_name, " PARTITION BY ANY
             ON ", centroid_table_name, " DIMENSION
             centroidsTable('baseball.kmeans_test_centroids')
           ) 
          WHERE clusterid != -1
         GROUP BY 1
       )
       SELECT * FROM sample (
         ON (SELECT ", ifelse(scaled,  " d.* ", " clusterid, d.* "), 
      "        FROM kmeansplot(
           ON ", scaled_table_name, " PARTITION BY ANY
           ON ", centroid_table_name, " DIMENSION
           centroidsTable('",centroid_table_name,"')
              ) ", ifelse(scaled, " d ", 
                          paste0( " kmp JOIN (", query_as_table, ") d ON (kmp.", idAlias, " = d.", idAlias, ")")), "
              WHERE clusterid != -1
            ) AS data PARTITION BY ANY
         ON stratum_counts AS summary DIMENSION
         CONDITIONONCOLUMN('clusterid')
         CONDITIONON(",conditionOnSql,")
         ApproximateSampleSize('", as.character(sampleSize), "')
      )"
    )
  }
  
  if (!includeId) {
    sql = paste0(
      "SELECT * FROM antiselect(
         ON 
           (",sql,"
           )
         EXCLUDE('",idAlias,"')
       )")
  }
  
  if(test) {
    return(sql)
  }else {
    return(toaSqlQuery(channel, sql))
  }
}