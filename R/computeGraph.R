#' Define an object corresponding to a graph in Aster database.
#' 
#' In Aster Database, to process graphs using SQL-GR, it is recommended to represent
#' a graph using two tables:
#' - Vertices table
#' - Edges table
#' Vertices table must contain a unique key so that each row represents a vertex.
#' Edges table must contain a pair of source and target keys (from vertices table)
#' so that each row represents an edge.
#' Both vertices and edges tables may contain additional columns representing 
#' optional attributes. For example if edges table has column 'weight' it can
#' correspond a graph with edge weights.
#' @param vertices A table, view, or query of a collection of vertices in the graph.
#' @param edges A table, view, or query of a collection of edges of the graph. 
#'   The collection must contain at least two lists of columns, one list that represents 
#'   the source vertex key and another list that represents the target vertex key.
#' @param directed logical: should edges be interpreted as directed?
#' @param key name of the column with vertex unique id (in the table \code{vertices}).
#' @param source name of the column with the from vertex (in the table \code{edges}).
#' @param target name of the column with the to vertex (in the tbale \code{edges}).
#' @param vertexAttrnames optionally, a list of columns containing vertex 
#'   attribute names.
#' @param edgeAttrnames optionally, a list of columns containing edge 
#'   attribute names.
#' @param vertexWhere optionally, a \code{SQL WHERE} clause to subset vertex table (use SQL 
#'   as if in \code{WHERE} clause but omit the keyword \code{WHERE}).
#' @param edgeWhere optionally, a \code{SQL WHERE} clause to subset edge table (use SQL 
#'   as if in \code{WHERE} clause but omit the keyword \code{WHERE}).
#' 
#' @export
#' 
toaGraph <- function(vertices, edges, directed=FALSE, 
                     key='id', source='source', target='target', 
                     vertexAttrnames=NULL, edgeAttrnames=NULL, 
                     vertexWhere = NULL, edgeWhere = NULL) {
  
  if(is.null(vertices) || is.null(edges))
    stop("Both vertices and edges must be defined.")
  
  z <- structure(list(vertices = vertices,
                      edges = edges,
                      key = key,
                      source = source,
                      target = target,
                      vertexAttrnames = vertexAttrnames,
                      edgeAttrnames = edgeAttrnames,
                      vertexWhere = vertexWhere,
                      edgeWhere = edgeWhere
  ),
  class = "toagraph")
 
  z 
}


#' Materialize Aster graph as network object in R.
#'
#' Results in \code{\link{network}} object representation of the graph 
#' stored in Aster tables. Usually in Aster database a graph is represented
#' using a pair of vertice and edge tables.
#' 
#' Use caution when computing network objects stored in Aster with this function 
#' as data may include considerable amount of vertices and edges which are too large to
#' load into a memory.  
#'
#'
#' @param channel connection object as returned by \code{\link{odbcConnect}}
#' @param graph an object of class \code{'toagraph'} referencing graph 
#'   tables in Aster database.
#' @param v vector or list of keys (corresponds to \code{vertex.names} attribute) of the vertices 
#'   to include in the graph. When not null this guarentees that no vertices outside of this list
#'   and no edges between vertices outsidef of this list will be included in the resulting network.
#' @param vertexWhere optionally, a \code{SQL WHERE} clause to subset vertex tablet. When not \code{NULL}
#'   it overrides \code{vertexWhere} condition from the \code{graph}.
#' @param edgeWhere optionally, a \code{SQL WHERE} clause to subset edge table. When not \code{NULL}
#'   it overrides \code{edgeWhere} condition from the \code{graph}.
#' @param allTables pre-built information about existing tables.
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \pkg{RODBC} 
#'   functions: \link{sqlQuery} and \link{sqlSave}).
#'   
#' @export
computeGraph <- function(channel, graph, v=NULL,
                         vertexWhere=graph$vertexWhere, 
                         edgeWhere=graph$edgeWhere, 
                         allTables=NULL, test=FALSE) {
  
  if (missing(graph) || !is.object(graph) || !inherits(graph, "toagraph"))
    stop("Graph object must be specified.")
  
  if (test && is.null(allTables))
    stop("Must provide allTables when test==TRUE.")
  
  isValidConnection(channel, test)
  
  isTableFlag = isTable(channel, c(vertices=graph$vertices, edges=graph$edges), allTables=allTables)
  
  if(!all(isTableFlag | is.na(isTableFlag)))
    stop("Both vertices and edges must be a table or view that exist.")
  
  # Handle vertex list if defined
  vertexWhere = addVerticesInVertexWhere(graph, v, vertexWhere)
  
  if(test) {
    emptyLine = "--"
    sqlText = ""
  }
  
  # Edges select
  sqlComment = "-- Edges Select"
  edgesSql = makeEdgesSql(graph, isTableFlag, vertexWhere, edgeWhere)

  if(test)
    sqlText = paste(sqlComment, edgesSql, sep='\n')
  else
    e = toaSqlQuery(channel, edgesSql, stringsAsFactors=FALSE)
  
  # Vertices select
  if (!is.null(graph$vertexAttrnames) && length(graph$vertexAttrnames) > 0) {
    sqlComment = "-- Vertices Select"
    verticesSql = makeVerticesSql(graph, isTableFlag, vertexWhere, FALSE)
      
    if(test)
      sqlText = paste(sqlText, paste(emptyLine, sqlComment, verticesSql, sep='\n'), sep=';\n')
    else
      v = toaSqlQuery(channel, verticesSql, stringsAsFactors=FALSE)
  }else
    v = NULL
  
  # result
  if (test) {
    return(sqlText)
  }else {
    net = makeNetworkResult(graph, v, e)

    return(net)
  }
  
}


#' Find the vertices not farther than a given limit from another fixed vertex, 
#' and create egographs (subgraphs) with the given order parameter.
#'
#' @param channel connection object as returned by \code{\link{odbcConnect}}
#' @param graph an object of class \code{'toagraph'} referencing graph 
#'   tables in Aster database.
#' @param ego the vertices for which the calculation of corresponding ego graphs is performed.
#' @param order	integer giving the order of the ego graph neighborhood.
#' @param mode character constant, it specifies how to use the direction of the edges if a directed graph is analyzed. 
#'   For \code{'out'} only the outgoing edges are followed, so all vertices reachable from the source vertex in at 
#'   most order steps are counted. For \code{'in'} all vertices from which the source vertex is reachable in at most 
#'   \code{order} steps are counted. \code{'all'} ignores the direction of the edges. This argument is ignored 
#'   for undirected graphs.
#' @param createDistanceAttr logical: indicates if vertices should receive attribute with the distance
#'   to ego graph centeral vertex.
#' @param distanceAttrname name of the vertex distance attribute.
#' @param vertexWhere SQL WHERE clause limiting data from the vertex table. This value when not null
#'   overrides corresponding value \code{vertexWhere} from \code{graph} (use SQL as if in WHERE clause but 
#'   omit keyword WHERE).
#' @param edgeWhere SQL WHERE clause limiting data from the edge table. This value when not null
#'   overrides corresponding value \code{edgeWhere} from \code{graph} (use SQL as if in WHERE clause but 
#'   omit keyword WHERE).
#' @param allTables pre-built information about existing tables.
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \pkg{RODBC} 
#'   functions: \link{sqlQuery} and \link{sqlSave}).
#' @export
computeEgoGraph <- function(channel, graph, ego, order=1, mode=c('in','out','both','all'),
                            createDistanceAttr=TRUE, distanceAttrname="ego.distance",
                            vertexWhere=graph$vertexWhere, edgeWhere=graph$edgeWhere,
                            allTables=NULL, test=FALSE) {
  
  if (missing(graph) || !is.object(graph) || !inherits(graph, "toagraph"))
    stop("Graph object must be specified.")
  
  if (test && is.null(allTables))
    stop("Must provide allTables when test==TRUE.")
  
  isValidConnection(channel, test)
  
  isTableFlag = isTable(channel, c(vertices=graph$vertices, edges=graph$edges), allTables=allTables)
  
  if(!all(isTableFlag | is.na(isTableFlag)))
    stop("Both vertices and edges must be a table or view that exist.")
  
  if(test) {
    emptyLine = "--"
    sqlText = ""
  }
  
  egoVertexWhere = addVerticesInVertexWhere(graph, ego, vertexWhere)
  sqlComment = "-- Create temp table of the shortest paths from ego vertices"
  sqlBeginTran = "BEGIN"
  shortestPathSql = paste0(
      "CREATE TEMP FACT TABLE egographtemp 
       DISTRIBUTE BY HASH(source) 
       AS
       SELECT source, target, distance FROM AllPairsShortestPath(
         ON (", makeVerticesSql(graph, isTableFlag, vertexWhere, FALSE), ") AS vertices PARTITION BY ", graph$key , "
         ON (", makeEdgesSql(graph, isTableFlag, vertexWhere, edgeWhere), ") AS edges PARTITION BY ", graph$source, "
         ON (", makeVerticesSql(graph, isTableFlag, egoVertexWhere, FALSE), ") AS sources PARTITION BY name
         TARGETKEY('",graph$target,"')
         DIRECTED('false')
         MAXDISTANCE('",order,"')
       )"
  )
  
  if(test) {
    sqlText = paste(sqlBeginTran, paste(emptyLine, sqlComment, shortestPathSql, sep='\n'), sep=';\n')
  }else {
    odbcSetAutoCommit(channel, autoCommit = FALSE)
    toaSqlQuery(channel, sqlBeginTran)
    toaSqlQuery(channel, shortestPathSql)
  }
  
  if (createDistanceAttr) {
    distanceColumnSql = paste0(", eg.distance ", "__distance_attr__")
    distance0ColumnSql = paste0(", 0 ", "__distance_attr__")
  }else {
    distanceColumnSql = ""
    distance0ColumnSql = ""
  }

  egoGraphs = list()
  for(i in 1:length(ego)) {
    
    key = ego[[i]]
    ego.graph = graph
    
    sqlComment = "-- Edges Select"
    edgesSql = paste0(
      "SELECT e.*
         FROM (", makeEdgesSql(ego.graph, isTableFlag, vertexWhere, edgeWhere), ") e 
        WHERE name1 IN (SELECT target FROM egographtemp WHERE source = '",key,"')
          AND name2 IN (SELECT target FROM egographtemp WHERE source = '",key,"')
       UNION
       SELECT e.*
         FROM (", makeEdgesSql(ego.graph, isTableFlag, vertexWhere, edgeWhere), ") e
        WHERE name1 = '",key,"'
           OR name2 = '",key,"'"
    )
    
    if(test)
      sqlText = paste(sqlText, paste(emptyLine, sqlComment, edgesSql, sep='\n'), sep=';\n')
    else
      e = toaSqlQuery(channel, edgesSql, stringsAsFactors=FALSE)
    
    if ((!is.null(ego.graph$vertexAttrnames) && length(ego.graph$vertexAttrnames) > 0) ||
        createDistanceAttr) {
      sqlComment = "-- Vertices Select"
      verticesSql = paste0(
        "SELECT v.*", distanceColumnSql, " 
           FROM egographtemp eg JOIN
                (", makeVerticesSql(ego.graph, isTableFlag, vertexWhere, FALSE), ") v ON (eg.target = v.name)
          WHERE eg.source = '",key,"'
         UNION
         SELECT v.*", distance0ColumnSql, " 
           FROM egographtemp eg JOIN
                (", makeVerticesSql(ego.graph, isTableFlag, vertexWhere, FALSE), ") v ON (eg.source = v.name)
          WHERE eg.source = '",key,"'"
      )
      
      if(test)
        sqlText = paste(sqlText, paste(emptyLine, sqlComment, verticesSql, sep='\n'), sep=';\n')
      else {
        v = toaSqlQuery(channel, verticesSql, stringsAsFactors=FALSE)
        if (createDistanceAttr) {
          ego.graph$vertexAttrnames = c(ego.graph$vertexAttrnames, distanceAttrname)
          names(v)[[length(v)]] = distanceAttrname
        }
      }
    }else
      v = NULL
    
    if(!test)
      egoGraphs[[i]] = makeNetworkResult(ego.graph, v, e)

  }
  
  sqlEndTran = "END"
  if(test) {
    sqlText = paste(sqlText, paste(emptyLine, sqlEndTran, sep='\n'), sep=';\n')
    
    return(sqlText)
  }else {
    toaSqlQuery(channel, sqlEndTran)
    odbcSetAutoCommit(channel, autoCommit = TRUE)
  
    return(egoGraphs)
  }
  
}


addVerticesInVertexWhere <- function(graph, v, vertexWhere) {
  
  if(!is.null(v) && length(v) > 0) {
    vertexValueWhere = paste0(graph$key, " IN (", makeSqlValueList(unlist(v)) , ")")
    if(is.null(vertexWhere))
      vertexWhere = vertexValueWhere
    else
      vertexWhere = paste0("(", vertexWhere, ") AND ", vertexValueWhere)
  }
  
  return(vertexWhere)
}


makeVerticesSql <- function(graph, isTableFlag, vertexWhere, keyOnlyFlag) {
  
  if(keyOnlyFlag) 
    selectList = graph$key
  else
    selectList = makeSqlColumnList(c(graph$key, graph$vertexAttrnames))
  
  paste0(
    "SELECT ", selectList, " 
       FROM ", makeFromClause(graph$vertices, isTableFlag[['vertices']], "t"),
    makeWhereClause(vertexWhere)
  )
}
 

makeEdgesSql <- function(graph, isTableFlag, vertexWhere, edgeWhere) {
  
  if (!is.null(vertexWhere)) {
    verticesSql = makeVerticesSql(graph, isTableFlag, vertexWhere, TRUE)
    
    if(!is.null(edgeWhere)) {
      edgeWhere = paste0(
        "(", edgeWhere, ") AND 
         ", graph$source, " IN (", verticesSql, ") AND 
         ", graph$target, " IN (", verticesSql, ")"
      )
    }else {
      edgeWhere = paste0(
        graph$source, " IN (", verticesSql, ") AND 
     ", graph$target, " IN (", verticesSql, ")"
      )
    }
  }
  
  paste0(
      "SELECT ", makeSqlColumnList(c(graph$source, graph$target, graph$edgeAttrnames)), " 
         FROM ", makeFromClause(graph$edges, isTableFlag[['edges']], "t"),
      makeWhereClause(edgeWhere)
  )
}


makeNetworkResult <- function(graph, v, e){

  net = network(e, matrix.type="edgelist", ignore.eval=FALSE)
  
  if(!is.null(v)) {
    net.v = data.frame(id=1:length(net$val), vertex.name=matrix(unlist(net$val), ncol=2, byrow=TRUE)[,2])
    net.v = merge(net.v, v, by.x="vertex.name", by.y=graph$key, all=FALSE, sort=FALSE)
      
    for(attrname in graph$vertexAttrnames) {
      network::set.vertex.attribute(net, attrname, net.v[, attrname], net.v[, "id"])
    }
  } 
  
  return(net)
}