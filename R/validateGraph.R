#' Validate graph data in Aster for consistency.
#' 
#' Aster uses the following pair of tables to store graph data:
#'   - vertices table with unique key and optional attributes
#'   - Edges table with source, target and optional attributes
#' Also see \code{\link{toaGraph}}
#' Graph validation checks for the following:
#' \itemize{
#'   \item Both tables exist (error returned).
#'   \item Edge sources exist in the vertices table.
#'   \item Edge targets exist in the vertices table.
#'   \item No duplicate vertices defined.
#'   \item No duplicate edges defined.
#'   \item No loops (self-directed edges) defined.
#' }
#' 
#' @param channel connection object as returned by \code{\link{odbcConnect}}
#' @param graph an object of class \code{'toagraph'} referencing graph 
#'   tables in Aster database.
#' @param weight logical or character: if logical then \code{TRUE} indicates using \code{'weight'} edge
#'   attribute, otherwise no weight used. If character then use as a name for the edge weight attribute. 
#'   The edge weight may apply with types \code{'clustering', 'shortestpath'} and centrality measures.
#' @param vertexWhere SQL WHERE clause limiting data from the vertex table. This value when not null
#'   overrides corresponding value \code{vertexWhere} from \code{graph} (use SQL as if in WHERE clause but 
#'   omit keyword WHERE).
#' @param edgeWhere SQL WHERE clause limiting data from the edge table. This value when not null
#'   overrides corresponding value \code{edgeWhere} from \code{graph} (use SQL as if in WHERE clause but 
#'   omit keyword WHERE).
#' @param allTables pre-built information about existing tables.
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \pkg{RODBC} 
#'   functions: \link{sqlQuery} and \link{sqlSave}).
#' 
#' @export
#' @examples 
#' if(interactive()) {
#' 
#' # initialize connection to Lahman baseball database in Aster 
#' conn = odbcDriverConnect(connection="driver={Aster ODBC Driver};
#'                          server=<dbhost>;port=2406;database=<dbname>;uid=<user>;pwd=<pw>")
#' 
#' # undirected graph
#' policeGraphUn = toaGraph(vertices="dallaspolice_officer_vertices", 
#'                          edges="dallaspolice_officer_edges_un", 
#'                          directed=FALSE, key="officer", 
#'                          source="officer1", target="officer2", 
#'                          vertexAttrnames = c("offense_count"),
#'                          edgeAttrnames = c("weight"))
#' validateGraph(conn, policeGraphUn)
#' 
#' # directed graph
#' policeGraphDi = toaGraph(edges="dallaspolice_officer_vertices", 
#'                          vertices="dallaspolice_officer_edges_di", 
#'                          directed=TRUE, key="officer", 
#'                          source="officer1", target="officer2", 
#'                          vertexAttrnames = c("offense_count"),
#'                          edgeAttrnames = c("weight"))
#' validateGraph(conn, policeGraphDi, weight=TRUE)
#' 
#' }
validateGraph <- function(channel, graph, weight=NULL,
                          vertexWhere=graph$vertexWhere, edgeWhere=graph$edgeWhere, 
                          allTables=NULL, test=FALSE) {
  
  if (missing(graph) || !is.object(graph) || !inherits(graph, "toagraph"))
    stop("Graph object must be specified.")
  
  if (test && is.null(allTables))
    stop("Must provide allTables when test==TRUE.")
  
  isValidConnection(channel, test)
  
  isTableFlag = isTable(channel, c(vertices=graph$vertices, edges=graph$edges), allTables=allTables)
  
  if(!all(isTableFlag | is.na(isTableFlag)))
    stop("Both vertices and edges must exist as tables or views.")
  
  verticesSql = makeVerticesSql(graph, isTableFlag, vertexWhere, TRUE)
  edgesSql = makeEdgesSql(graph, isTableFlag, vertexWhere, edgeWhere)
  
  emptyLine = "--"
  
  if(test)
    sqlText = ""
  
  # validate edges between non-existent vertices
  sqlComment = "-- Check non-existent source vertices"
  sql = paste0(
    "SELECT COUNT(*) FROM (", edgesSql, ") es WHERE ", graph$source, " NOT IN 
       (SELECT ", graph$key, " FROM (", verticesSql, ") v )"
  )
  if(test) {
    sqlText = paste(sqlComment, sql, sep='\n')
  }else {
    countNonExistentSources = toaSqlQuery(channel, sql)[[1,1]]
  }
  
  sqlComment = "-- Check non-existent target vertices"
  sql = paste0(
    "SELECT COUNT(*) FROM (", edgesSql, ") es WHERE ", graph$target, " NOT IN 
       (SELECT ", graph$key, " FROM (", verticesSql, ") v )"
  )
  if(test) {
    sqlText = paste(sqlText, paste(emptyLine, sqlComment, sql, sep='\n'), sep=';\n')
  }else {
    countNonExistentTargets = toaSqlQuery(channel, sql)[[1,1]]
  }
  
  
  # validate duplicate vertices in vertices table
  sqlComment = "-- Check duplicate vertices"
  sql = paste0(
      "SELECT COUNT(*) FROM (SELECT ", graph$key, " 
         FROM (", verticesSql, ") vs 
        GROUP BY 1 HAVING COUNT(*) > 1) t"
  )
  if(test) {
    sqlText = paste(sqlText, paste(emptyLine, sqlComment, sql, sep='\n'), sep=';\n')
  }else {
    countDupvertices = toaSqlQuery(channel, sql)[[1,1]]
  }
  
  
  # validate duplicate edges in edges table
  sqlComment = "-- Check duplicate edges"
  if (graph$directed) {
    sql = paste0(
      "SELECT COUNT(*) FROM (
         SELECT ", graph$source, ", ", graph$target, " 
           FROM (", edgesSql, ") es 
          GROUP BY 1,2 
         HAVING COUNT(*) > 1
       ) t2"
    )
  }else {
    sql = paste0(
      "SELECT COUNT(*) FROM (
         SELECT source, target 
           FROM (SELECT ", graph$source, " source, ", graph$target, " target FROM (", edgesSql, ") es1 
                  UNION ALL
                 SELECT ", graph$target, " source, ", graph$source, " target FROM (", edgesSql, ") es2
                ) t
          GROUP BY 1,2 
         HAVING COUNT(*) > 1
      ) t2"
    )
  }
  if(test) {
    sqlText = paste(sqlText, paste(emptyLine, sqlComment, sql, sep='\n'), sep=';\n')
  }else {
    countDupEdges = toaSqlQuery(channel, sql)[[1,1]]
  }
  
  
  # validate self-loops in edges table
  sqlComment = "-- Check self-loops"
  sql = paste0(
    "SELECT COUNT(*) 
       FROM (", edgesSql, ") es 
      WHERE ", graph$source, " = ", graph$target
  )
  if(test) {
    sqlText = paste(sqlText, paste(emptyLine, sqlComment, sql, sep='\n'), sep=';\n')
  }else {
    countLoops = toaSqlQuery(channel, sql)[[1,1]]
  }
  
  # validate weight attribute (if present)
  # weight attribute if present
  weight = parseWeightArgument(graph, weight)
  
  sqlComment = '-- Check non-negative weight'
  if (!is.null(weight)) {
    sql = paste0(
      "SELECT COUNT(*) 
         FROM (", edgesSql, ") es
        WHERE ", weight, " <= 0"
    )
    if(test) {
      sqlText = paste(sqlText, paste(emptyLine, sqlComment, sql, sep='\n'), sep=';\n')
    }else {
      countNonpositiveWeight = toaSqlQuery(channel, sql)[[1,1]]
    }
  }else {
    countNonpositiveWeight = NA
  }
  
  
  if(test) {
    sqlText = paste0(sqlText, ';')
    return(sqlText)
  }
  
  result = data.frame(
    check = c("Non-existent edge sources", "Non-existent edge targets", 
              "Duplicate vertices", "Duplicate edges", "Loops", "Non-positive weight"),
    value = c(countNonExistentSources, countNonExistentTargets, countDupvertices, 
              countDupEdges, countLoops, countNonpositiveWeight),
    flag = c(countNonExistentSources>0, countNonExistentTargets>0, countDupvertices>0, 
             countDupEdges>0, countLoops>0, countNonpositiveWeight>0),
    stringsAsFactors = FALSE
  )
  
  return(result)
  
}