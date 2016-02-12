#' Plot an Aster graph object comprised of the vertice and edge tables.
#' 
#' Function for obtaining and plotting graph (network) objects using ggplot2 and ggnet2 functions.
#' 
#' @param channel connection object as returned by \code{\link{odbcConnect}}
#' @param graph an object of class \code{'toagraph'} referencing graph 
#'   tables in Aster database.
#' @param allTables pre-built information about existing tables.
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \pkg{RODBC} 
#'   functions: \link{sqlQuery} and \link{sqlSave}).
#'    
#' @return a ggplot object
#' @export
#' @examples 
#' if(interactive()) {
#' # initialize connection to Lahman baseball database in Aster 
#' conn = odbcDriverConnect(connection="driver={Aster ODBC Driver};
#'                          server=<dbhost>;port=2406;database=<dbname>;uid=<user>;pwd=<pw>")
#' 
#' showGraph(conn, "graph.films_vertices", "graph.films_edges")
#' 
#' }
showGraph <- function(channel, graph, allTables=NULL, test=FALSE) {
  
  if (test && is.null(allTables))
    stop("Must provide allTables when test==TRUE.")
  
  net = computeGraph(channel, graph, allTables = allTables, test = test)
  
  if (test)
    return(net)
  
  p = GGally::ggnet2(net) +
    theme_tufte()
  
  return(p)
}