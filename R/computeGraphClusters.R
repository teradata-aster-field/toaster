#' Pefrom graph clustering of various types.
#' 
#' Graph clustering (or decomposition) divides graph into
#' set of subgraphs that span whole graph. Depending on the \code{type} argument
#' the subgraphs coudl be either non-intersecting or overlapping.
#' Available types of decomposition include finding connected
#' componenets, modularity clustering.
#' 
#' @param channel connection object as returned by \code{\link{odbcConnect}}.
#' @param graph an object of class \code{'toagraph'} referencing graph 
#'   tables in Aster database.
#' @param type specifies type of clustering or community detection to perform.
#' @param createMembership logical indicates if vertex cluster membership table should be created (see \code{membershipTableName}).
#'   Currently, you must set it to \code{TRUE} if cluster membership data (see \code{includeMembership}) expected in the result.
#'   Also, required if operations that create graphs corresponding to some of the clusters to be performed later.
#' @param includeMembership logical indicates if result should contain vertex cluster membership information. Currently,
#'   only supported when \code{createMembership} is \code{TRUE}.
#'   WARNING: including cluster membership may result in very large data set returned from Aster into memory. 
#' @param weight logical or character: if logical then \code{TRUE} indicates using \code{'weight'} edge
#'   attribute, otherwise no weight used. If character then use as a name for the edge weight attribute. 
#'   The edge weight may apply with types \code{'clustering', 'shortestpath'} and centrality measures.
#' @param vertexWhere optionally, a \code{SQL WHERE} clause to subset vertex table. When not \code{NULL}
#'   it overrides \code{vertexWhere} condition from the \code{graph}.
#' @param edgeWhere optionally, a \code{SQL WHERE} clause to subset edge table. When not \code{NULL}
#'   it overrides \code{edgeWhere} condition from the \code{graph}.
#' @param distanceTableName this table will contain distances between vertices (or other corresponding metrics associated with 
#'   community detection algorithm chosen). By default, random table name that begins with \code{toa_temp_graphcluster_distance}
#'   is generated.
#' @param membershipTableName when \code{createMembership} is \code{TRUE} then this table will contain vertex cluster membership 
#'   information. By default, random table name that begins with \code{toa_temp_graphcluster_membership} is generated. This 
#'   argument is ignored when \code{createMembership} is \code{FALSE}.
#' @param schema name of Aster schema for the table name arguments \code{distanceTableName} and \code{membershipTableName}.
#'   There are two distinct approaches to providing table names: one that uses explicity schema name using this argument and
#'   another when table names already contain schema followed by dot and table name. The latter method is not applicable when
#'   generating randon table name with schema.
#' @param allTables pre-built information about existing tables.
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \pkg{RODBC} 
#'   functions: \link{sqlQuery} and \link{sqlSave}).
#' @param ... other arguments passed on to Aster graph functions except for \code{EDGEWEIGHT} argument -
#'   use argument \code{weight} instead. Aster function areguments are not case-sensetive.
#' 
#' @return \code{computeGraphClusters} returns an object of class \code{"toacommunities"} (compatible with 
#'   both class \code{"\link[igraph]{communities}"} and the value returned by \code{\link[igraph]{clusters}} - 
#'   all from the package \pkg{igraph}). It is a list with the following components:
#'   \describe{
#'     \item{\code{membership}}{numeric vector giving the cluster (component or community) id to which each vertex belongs.}
#'     \item{\code{csize} and \code{sizes}}{numeric vector giving the sizes of the clusters.}
#'     \item{\code{no} and \code{length}}{numeric constant, the number of clusters.}
#'     
#'     \item{\code{algorithm}}{gives the name of the algorithm that was used to calculate the community structure.}
#'     
#'     \item{\code{id}}{integer vector of cluster ids from 1 to number \code{no}.}
#'     \item{\code{componentid}}{character vector of cluster names (or component ids) where names are derived from the 
#'       cluster elements and naming convention differs for each community type.}
#'     \item{\code{distance}}{numeric vector of average distances within clusters.}
#'     \item{\code{diameter}}{numeric vector of the maximum distances with clusters.}
#'     
#'     \item{\code{graph}}{original graph object that identifies a graph for which clusters are crated.}
#'     \item{\code{weight}}{see argument \code{weight} above.}
#'     \item{\code{vertexWhere}}{see argument \code{vertexWhere} above.}
#'     \item{\code{edgeWhere}}{see argument \code{edgeWhere} above.}
#'     \item{\code{distanceTableName}}{Aster table name containing graph distances (applies to connected components only).}
#'     \item{\code{membershipTableName}}{(optional) Aster table name containing graph vertex to cluster memberships.}
#'     \item{\code{time}}{An object of class \code{proc_time} with user, system, and total elapsed times
#'     for the \code{computeGraphClusters} function call.}
#'   }
#'   
#' @export
#' @examples 
#' if(interactive()) {
#' 
#' # undirected graph
#' policeGraphUn = toaGraph("dallaspolice_officer_vertices", "dallaspolice_officer_edges_un", 
#'      directed = FALSE, key = "officer", source = "officer1", target = "officer2", 
#'      vertexAttrnames = c("offense_count"), edgeAttrnames = c("weight"))
#'      
#' communities = computeGraphClusters(conn, policeGraphUn, type="connected", 
#'                                    createMembership = TRUE, includeMembership = TRUE,
#'                                    distanceTableName = "public.shortestpathdistances",
#'                                    membershipTableName = "public.clustermembership")
#'                                    
#' # get first 5 largest connected components as graphs
#' cluster_graphs = computeGraphClustersAsGraphs(conn, communities = communities, ids = 1:5)
#' 
#' # visualize component 2
#' library(GGally)
#' ggnet2(cluster_graphs[[2]], node.label="vertex.names", node.size="offense_count", 
#'        node.color="color", legend.position="none")
#' 
#' # compute connected components for certain type of subgraph that 
#' # includes only verteics that start with the letters
#' communities2 = computeGraphClusters(conn, policeGraphUn, type="connected", membership = TRUE,
#'                                     distanceTableName = "public.shortestpathdistances",
#'                                     vertexWhere = "officer ~ '[A-Z].*'", 
#'                                     edgeWhere = "weight > 0.36")
#' }
computeGraphClusters <- function(channel, graph, type='connected', 
                                 createMembership=FALSE, includeMembership=FALSE, 
                                 weight=FALSE, vertexWhere=graph$vertexWhere, edgeWhere=graph$edgeWhere,
                                 distanceTableName=NULL, membershipTableName=NULL, schema=NULL,
                                 allTables=NULL, test=FALSE, ...) {
  
  # match argument values
  type = match.arg(type, c('connected', 'modularity'))
  
  if (missing(graph) || !is.object(graph) || !inherits(graph, "toagraph"))
    stop("Graph object must be specified.")
  
  if (includeMembership && !createMembership)
    stop("Cluster membership info is available only when createMembership=TRUE.")
  
  if (test && is.null(allTables))
    stop("Must provide allTables when test==TRUE.")
  
  isValidConnection(channel, test)
  
  isTableFlag = isTable(channel, list(vertices=graph$vertices, edges=graph$edges), allTables=allTables)
  
  if(!all(isTableFlag | is.na(isTableFlag)))
    stop("Both vertices and edges must exist as tables or views.")
  
  # weight attribute if present
  weight = parseWeightArgument(graph, weight)
  
  argsSql = makeGraphFunctionArgumentsSql(...)
  
  if(test)
    sqlText = ""
  
  if(type=='connected') {
    clusters = computeConnectedClusters(channel, graph, isTableFlag, 
                                        createMembership, includeMembership, 
                                        distanceTableName, membershipTableName, schema,
                                        weight, vertexWhere, edgeWhere, 
                                        allTables, test, argsSql)
  }
  
  return(clusters)
}


computeConnectedClusters <- function(channel, graph, isTableFlag, 
                                     createMembership, includeMembership, 
                                     distanceTableName, membershipTableName, schema,
                                     weight, vertexWhere, edgeWhere, 
                                     allTables, test, argsSql) {
  
  ptm = proc.time()
  
  if (is.null(distanceTableName))
    distanceTableName = makeTempTableName('graphcluster_distances', 30, schema = schema)
  else if (!is.null(schema))
    distanceTableName = paste0(schema, ".", distanceTableName)
  
  if(createMembership){
    if (is.null(membershipTableName))
      membershipTableName = makeTempTableName('graphcluster_membership', 30, schema = schema)
    else if (!is.null(schema))
      membershipTableName = paste0(schema, ".", membershipTableName)
  }else
    membershipTableName = NULL
    
    
  emptyLine = "--"
  
  if(test)
    sqlText = ""
  
  # make shortest path table that contains distances between all connected paths
  sqlComment = "-- Shortest Paths Table"
  sqlDrop = paste("DROP TABLE IF EXISTS", distanceTableName)
  sql = makeAllPairsShortestPathSql(distanceTableName, graph, isTableFlag, weight, vertexWhere, edgeWhere, argsSql)
  
  if(test) {
    sqlText = paste(sqlComment, sqlDrop, sep='\n')
    sqlText = paste(sqlText, sql, sep=';\n')
  }else {
    toaSqlQuery(channel, sqlDrop)
    toaSqlQuery(channel, sql)
  }
  
  # compute connected components and their properites
  sqlComment = "-- Compute connected components with properties"
  sql = paste0(
    "SELECT ROW_NUMBER() OVER (ORDER BY vertex_count DESC, componentid) id, * 
       FROM 
         (SELECT componentid, 
                 COUNT(CASE WHEN distance=0 THEN distance ELSE NULL END) vertex_count,
                 COUNT(CASE WHEN distance=1 THEN 1 ELSE NULL END)/2  edge_count,
                 CASE WHEN SUM(distance)=0 THEN 0 
                      ELSE SUM(distance)/COUNT(CASE WHEN distance!=0 THEN distance ELSE NULL END)
                 END avg_distance,
                 MAX(distance) diameter
           FROM 
             (", makeAllConnectedPairsWithDistanceSql(graph, isTableFlag, vertexWhere, distanceTableName)," 
             ) t1
         GROUP BY 1 ) t2
    ORDER BY 1"
  )
  
  if(test) {
    sqlText = paste(sqlText, emptyLine, sqlComment, sql, sep=';\n')
  }else {
    data = toaSqlQuery(channel, sql, stringsAsFactors=FALSE)
  }
  
  if(createMembership) {
    
    # compute membership
    sqlComment = "-- Compute vertex membership"
    sqlDrop = paste("DROP TABLE IF EXISTS", membershipTableName)
    sqlCreate = makeConnectedClusterMembershipCreateSql(graph, isTableFlag, vertexWhere, distanceTableName, membershipTableName)
    sqlSelect = paste("SELECT * FROM", membershipTableName, "ORDER BY vertex")
    
    if(test) {
      sqlText = paste(sqlText, emptyLine, sqlComment, sqlDrop, sqlCreate, sep=';\n')
      if (includeMembership)
        sqlText = paste(sqlText, sqlSelect, sep=';\n')
    }else {
      toaSqlQuery(channel, sqlDrop)
      toaSqlQuery(channel, sqlCreate)
      if (includeMembership)
        memberdata = toaSqlQuery(channel, sqlSelect, stringsAsFactors=FALSE)
      else
        memberdata = NULL
    }
  }else
    memberdata = NULL
  
  # return sql
  if(test) {
    sqlText = paste0(sqlText, ';')
    return(sqlText)
  }
  
  result = makeGraphClusterResult(data, memberdata, 'connected', graph, weight, vertexWhere, edgeWhere, 
                                  distanceTableName, membershipTableName, ptm)
  
  return(result)
}


makeAllConnectedPairsWithDistanceSql <- function(graph, isTableFlag, vertexWhere, distanceTableName) {
  
  sql = paste0(
    "SELECT source, target, distance,
                     MIN(target) OVER (PARTITION BY source) componentid
                FROM 
                  (SELECT ", graph$key, " source, ", graph$key, " target, 0 distance 
                     FROM (",makeVerticesSql(graph, isTableFlag, vertexWhere, TRUE),") t
                    UNION 
                   SELECT source, target, distance FROM ", distanceTableName, "
                  ) t"
  )
  
}


makeAllPairsShortestPathSql <- function(distanceTableName, graph, isTableFlag, weight, vertexWhere, edgeWhere, argsSql) {
  
  sql = paste0(
    "CREATE FACT TABLE ", distanceTableName, "
     DISTRIBUTE BY HASH(source) AS
     SELECT source, target, distance from AllPairsShortestPath(
       ON (", makeVerticesSql(graph, isTableFlag, vertexWhere, FALSE), ") AS vertices PARTITION BY ", graph$key, "
       ON (", makeEdgesSql(graph, isTableFlag, vertexWhere, edgeWhere), ") AS edges PARTITION BY ", graph$source, "
       TARGETKEY('",graph$target,"')
       DIRECTED('",ifelse(graph$directed,"true","false"),"')
     ",ifelse(is.null(weight), "", paste0("EDGEWEIGHT('",weight,"')")), "
       MAXDISTANCE('-1')",
       argsSql, "
     )"
  )
}


makeConnectedClusterMembershipSelectSql <- function(graph, isTableFlag, vertexWhere, distanceTableName) {
  
  sql = paste0(
    "SELECT component.id, membership.componentid, membership.target vertex 
         FROM (SELECT ROW_NUMBER() OVER (ORDER BY vertex_count DESC, componentid) id, * 
       FROM 
         (SELECT componentid, 
                 COUNT(CASE WHEN distance=0 THEN distance ELSE NULL END) vertex_count
            FROM 
             (", makeAllConnectedPairsWithDistanceSql(graph, isTableFlag, vertexWhere, distanceTableName)," 
             ) t1
         GROUP BY 1 ) t2) component JOIN
         (", makeAllConnectedPairsWithDistanceSql(graph, isTableFlag, vertexWhere, distanceTableName)," 
             ) membership ON (component.componentid = membership.componentid)
       WHERE membership.source = membership.componentid"
  )
}

makeConnectedClusterMembershipCreateSql <- function(graph, isTableFlag, vertexWhere, 
                                              distanceTableName, membershipTableName) {
  
  sql = paste0(
      "CREATE FACT TABLE ", membershipTableName, "
       DISTRIBUTE BY HASH(id) AS
      ", makeConnectedClusterMembershipSelectSql(graph, isTableFlag, vertexWhere, distanceTableName)
    )
}


makeGraphClusterResult <- function(data, memberdata, algorithm, graph, weight, vertexWhere, edgeWhere, 
                                   distanceTableName, membershipTableName, ptm) {
  
  if (!is.null(memberdata)) {
    membership = memberdata$id
    names(membership) = memberdata$componentid
  }else
    membership = NULL
  
  number_of_clusters = dim(data)[[1]]
  cluster_sizes = data$vertex_count
  
  z <- structure(list(# cluster list
                      membership=membership,
                      csize=cluster_sizes,
                      no=number_of_clusters,
                      
                      # community class
                      length=number_of_clusters,
                      sizes=cluster_sizes,
                      algorithm=algorithm,
                      
                      # graph clusters info and stats
                      id=data$id,
                      componentid=data$componentid,
                      distance=data$avg_distance,
                      diameter=data$diameter,
                      
                      # graph info
                      graph=graph,
                      weight=weight,
                      vertexWhere=vertexWhere,
                      edgeWhere=edgeWhere,
                      
                      # execution context info
                      distanceTableName=distanceTableName,
                      membershipTableName=membershipTableName,
                      time=proc.time() - ptm
  ),
  class = c("toacommunities", "communities"))
  
  return (z)
}


#' Creates list of graphs for each specified component.
#' 
#' Based on the decomposition specified by \code{communities} object (see \code{\link{computeGraphClusters}}) 
#' materiazlies produced clusters as graph objects from Aster graph tables.
#' 
#' @param channel connection object as returned by \code{\link{odbcConnect}}.
#' @param communities community object returned by \code{\link{computeGraphClusters}}.
#' @param ids integer vector with cluster integer ids (from \code{1} to \code{N}, where \code{N} is the number of clusters). 
#'   At least one value for this or \code{componentids} must be specified.
#' @param componentids character vector with cluster component ids assigned during community 
#'   generation with \code{\link{computeGraphClusters}}. These component ids are derived from 
#'   one of the vertex name (likely first vertex when ordered alphabetically). At least one value 
#'   for this or \code{ids} must be specified.
#' @param allTables pre-built information about existing tables.
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \pkg{RODBC} 
#'   functions: \link{sqlQuery} and \link{sqlSave}).
#' @param parallel logical: enable parallel calls to Aster database. This option requires parallel 
#'   backend enabled and registered (see in examples). Parallel execution requires ODBC \code{channel} 
#'   obtained without explicit password: either with \code{\link{odbcConnect}(dsn)} or 
#'   \code{\link{odbcDriverConnect}} calls, but not with \code{\link{odbcConnect}(dsn, user, password)}.
#'  
#' @return list of \code{\link{network}} objects materializing specified clusters (communities) represented by 
#'   \code{communities} object.
#' @export 
#' 
#' @examples 
#' if(interactive()) {
#' 
#' # undirected graph
#' policeGraphUn = toaGraph("dallaspolice_officer_vertices", "dallaspolice_officer_edges_un", 
#'      directed = FALSE, key = "officer", source = "officer1", target = "officer2", 
#'      vertexAttrnames = c("offense_count"), edgeAttrnames = c("weight"))
#'      
#' communities = computeGraphClusters(conn, policeGraphUn, type="connected", 
#'                                    createMembership = TRUE, includeMembership = TRUE,
#'                                    distanceTableName = "public.shortestpathdistances",
#'                                    membershipTableName = "public.clustermembership")
#'                                    
#' # get first 5 largest connected components as graphs
#' cluster_graphs = computeGraphClustersAsGraphs(conn, communities = communities, ids = 1:5)
#' 
#' # visualize component 2
#' library(GGally)
#' ggnet2(cluster_graphs[[2]], node.label="vertex.names", node.size="offense_count", 
#'        node.color="color", legend.position="none")
#' 
#' }  
#'   
computeGraphClustersAsGraphs <- function(channel, communities, ids=NULL, componentids=NULL, 
                                         allTables=NULL, test=FALSE, parallel=FALSE) {
  
  if (missing(communities) || !is.object(communities) ||
      !inherits(communities, "toacommunities") ||
      !inherits(communities, "communities")) {
    stop("Graph communities object must be specified.")
  }
  
  # either ids or componentids must exist and be non-empty
  if ((is.null(ids) || length(ids) == 0) &&
      (is.null(componentids) || length(componentids) == 0))
    stop("One of the arguments 'ids' or 'componentids' must be specified and contain at least one value.")
  
  if (is.null(communities$membershipTableName)) 
    stop("Membership table is not defined and may not exist. Please, rerun graph clustering with createMembership=TRUE.")
  
  graph = communities$graph
  
  if (is.null(graph) || !is.object(graph) || !inherits(graph, "toagraph"))
    stop("Graph object must be specified.")
  
  if (test && is.null(allTables))
    stop("Must provide allTables when test==TRUE.")
  
  isValidConnection(channel, test)
  
  isTableFlag = isTable(channel, list(vertices=graph$vertices, edges=graph$edges), allTables=allTables)
  
  if (is.null(ids) || length(ids) == 0) {
    ids = integer(0)
  }
  
  # replace componentids with equivalent ids
  if (!is.null(componentids) && length(componentids) > 0)
    ids = c(ids, communities$id[which(communities$componentid %in% componentids)])
  
  vertex_where_template = paste(graph$key, 
                                "IN (SELECT vertex FROM", communities$membershipTableName, 
                                     "WHERE id = <%%%id%%%>)")
  
  if (!parallel) {
    result = foreach(id = ids, .combine=c, .packages=c('RODBC')) %do% {
      vertex_where_clause = gsub('<%%%id%%%>', as.character(id), vertex_where_template, fixed=TRUE)
      g = computeGraphInternal(channel, graph, vertexWhere=vertex_where_clause,
                               isTableFlag=isTableFlag)
      list(g)
    }
  }else {
    result = foreach(id = ids, .combine=c, .packages=c('RODBC'),
                     .errorhandling='stop') %dopar% {
      vertex_where_clause = gsub('<%%%id%%%>', as.character(id), vertex_where_template, fixed=TRUE)
      parChan = odbcReConnect(channel)
      g = computeGraphInternal(parChan, graph, vertexWhere=vertex_where_clause, 
                               isTableFlag=isTableFlag, closeOnError=TRUE)
      close(parChan)
      list(g)
    }
  }
  
  return(result)
} 