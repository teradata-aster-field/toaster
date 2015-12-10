#' @demoTitle baseball-kmeans
#' 
#' Demo kmeans clustering of baseball batting Aster table
#'
#' To install and use baseball demo dataset in Aster:
#'
#' 1. download baseball.zip from
#'   https://raw.githubusercontent.com/wiki/teradata-aster-field/toaster/downloads/baseball.zip
#'   and extract files into directory of your choice
#' 2. in the directory run the script to create data set in Aster
#'   sh load_baseball_data.sh -d mydbname -U username -w mypassword 
#' 3. create Aster ODBC DSN on your desktop
#'   see https://bitbucket.org/grigory/toaster/wiki/Home#markdown-header-odbc-driver-and-dns

library(toaster)

## utility input function
readlineDef <- function(prompt, default) {
  if (!is.null(prompt))
    prompt = paste0(prompt, "[", default, "]: ")
  else 
    prompt = paste0(prompt, ": ")
  
  result = readline(prompt)
  if (result == "") 
    return (default)
  else
    return (result)
}

## utility connection function
connectWithDSNToAster <- function(dsn=NULL) {
  dsn = readlineDef("Enter Aster ODBC DSN: ", dsn)
  
  tryCatch(close(conn), error=function(err) {NULL})
  
  conn = tryCatch({
    conn = odbcConnect(dsn)
    odbcGetInfo(conn)
    return (conn)
  }, error=function(err) {
    stop(paste("Can't connect to Aster - check DSN '", dsn, "'"))
  })
}

## utility pause function
pause <- function() {
  cat("Press ENTER/RETURN/NEWLINE to continue.")
  readLines(n=1)
  invisible()
}

## connect to Aster first
conn = connectWithDSNToAster()

## must be connected to baseball dataset
if(!all(isTable(conn, c('batting')))) {
  stop("Must connect to baseball dataset and tables must exist.")
}

# run kmeans in Aster
km = computeKmeans(conn, "batting", centers=3, iterMax = 25,  tableInfo = batting_info, 
                   aggregates = c("COUNT(*) cnt", "AVG(g) avg_g", "AVG(r) avg_r", "AVG(h) avg_h","AVG(ab) avg_ab"),
                   id="playerid || '-' || stint || '-' || teamid || '-' || yearid", include=c('g','r','h','ab'),
                   scaledTableName='kmeans_test_scaled', centroidTableName='kmeans_test_centroids', schema='public',
                   where="yearid > 2000", test=FALSE)

pause()

createCentroidPlot(km, format="line", coordFlip = FALSE)
pause()

createCentroidPlot(km, format="line", groupByCluster=FALSE, coordFlip = FALSE)
pause()

createCentroidPlot(km, format="bar", coordFlip = FALSE)
pause()

createCentroidPlot(km, format="bar", groupByCluster=FALSE, coordFlip = FALSE)
pause()

createCentroidPlot(km, format="bar_dodge", coordFlip = FALSE)
pause()

createCentroidPlot(km, format="bar_dodge", groupByCluster=FALSE, coordFlip = FALSE)
pause()

createCentroidPlot(km, format="heatmap", coordFlip = FALSE)
pause()

createClusterPlot(km)
pause()

createClusterPlot(km, colorByCluster = FALSE)
pause()

kms = computeClusterSample(conn, km, '0.5', test=FALSE)

pause()

createClusterPairsPlot(kms)
