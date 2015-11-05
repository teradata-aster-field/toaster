# Prototype of kmeans function in toaster
computeKmeans <- function(channel, tableName, centers, id, dimNames, 
                          aggregates, where="1 = 1", scale=FALSE, drop=TRUE, test=FALSE) {
  
  viewName = paste0(tableName, "_view")
  outputName = paste0(tableName, "_clusters")
  
  whereClause = paste(" where ", paste0("p.", dimNames, " is not null", collapse=" and "), 
                      " and ", where)
  
  createViewSql = paste0(
    "create or replace view ", viewName, 
    " as select ", id, ", ",  paste(dimNames, collapse=", "),
    "     from ", tableName, " p ", whereClause)
  sqlQuery(conn, createViewSql)
  
  # serverName = odbcGetInfo(conn)["Server_Name"]
  kmeansSql = paste0(
    "SELECT *
    FROM kmeans
    (
    ON (SELECT 1)
    PARTITION BY 1
    INPUTTABLE('",viewName,"')
    OUTPUTTABLE('",outputName,"')
    NUMBERK('",centers,"')
    THRESHOLD('0.001')
    MAXITERNUM('100')
    )")
  
  if (drop) 
    sqlQuery(conn, paste0("DROP TABLE ", outputName))
  
  rs = sqlQuery(conn, kmeansSql)
  
  clustersWithValuesSql = paste0(
    "SELECT c.clusterid, c.means means, ", paste(aggregates, collapse=", "), 
    "  FROM 
    (SELECT *
    FROM kmeansplot (
    ON ", viewName, " as input_data PARTITION BY ANY
    ON ", outputName, " DIMENSION
    centroidsTable('",outputName,"')
    )) wc
    JOIN ", tableName, " p on (wc.permitno = p.permitno)
    JOIN ", outputName, " c on (wc.clusterid = c.clusterid) ",
  whereClause, 
" GROUP BY c.clusterid, c.means")
  
  data = sqlQuery(conn, clustersWithValuesSql, stringsAsFactors=FALSE)
  
  clusters = data.frame(t(matrix(
    unlist(strsplit(as.vector(data$means), split = " ")), 
    ncol = length(data$means), nrow = 2)), stringsAsFactors=FALSE)
  data$lat = as.numeric(clusters$X1)
  data$lon = as.numeric(clusters$X2)
  
  return(data)
}