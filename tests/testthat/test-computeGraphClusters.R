context("computeGraphClusters-connectivity")

test_that("computeGraphClusters throws errors", {
  
  expect_error(computeGraphClusters(type = 'not a type'),
               ".*'arg' should be one of \"connected\", \"modularity\"")
  
  expect_error(computeGraphClusters(NULL),
               "Graph object must be specified.")
  
  expect_error(computeGraphClusters(NULL, NULL),
               "Graph object must be specified.")
  
  expect_error(computeGraphClusters(NULL, 1),
               "Graph object must be specified.")
  
  expect_error(computeGraphClusters(NULL, "not graph"),
               "Graph object must be specified.")
  
  expect_error(computeGraphClusters(NULL, structure(list(data=c(1,2,3)),
                                                    class = c("xxxxx"))),
               "Graph object must be specified.")
  
  expect_error(computeGraphClusters(NULL, toaGraph("vs", "es"), includeMembership = TRUE),
               "Cluster membership info is available only when createMembership=TRUE.")
  
  expect_error(computeGraphClusters(NULL, toaGraph("vs", "es"), test=TRUE),
               "Must provide allTables when test==TRUE.")
  
  expect_error(computeGraphClusters(NULL, toaGraph("vs", "es"), 
                          allTables = data.frame(TABLE_NAME=c("dallaspolice_officer_vertices","dallaspolice_officer_edges_di")),
                          test=TRUE),
               "Both vertices and edges must exist as tables or views.")
  
  expect_error(computeGraphClusters(NULL, toaGraph("vs", "es"), weight=TRUE, 
                                     allTables = data.frame(TABLE_NAME=c("vs","es"), stringsAsFactors = FALSE), 
                                     test=TRUE),
               "No edge attribute 'weight' found in graph.")
  
  expect_error(computeGraphClusters(NULL, toaGraph("vs", "es"), weight='weight',
                                     allTables = data.frame(TABLE_NAME=c("vs","es"), stringsAsFactors = FALSE), 
                                     test=TRUE),
               "No edge attribute 'weight' found in graph.")
  
  expect_error(computeGraphClusters(NULL, toaGraph("vs", "es"), weight="notweight",
                                     allTables = data.frame(TABLE_NAME=c("vs","es"), stringsAsFactors = FALSE), 
                                     test=TRUE),
               "No edge attribute 'notweight' found in graph.")
  
})

test_that("computeGraphClustersAsGraphs throws errors", {
  
  expect_error(computeGraphClustersAsGraphs(NULL, NULL),
               "Graph communities object must be specified.")
  
  expect_error(computeGraphClustersAsGraphs(NULL, 1),
               "Graph communities object must be specified.")
  
  expect_error(computeGraphClustersAsGraphs(NULL, "string"),
               "Graph communities object must be specified.")
  
  expect_error(computeGraphClustersAsGraphs(NULL, structure(list(data=c(1,2,3)))),
               "Graph communities object must be specified.")
  
  expect_error(computeGraphClustersAsGraphs(NULL, structure(list(data=c(1,2,3)),
                                                            class = c("xxxxx"))),
               "Graph communities object must be specified.")
  
  expect_error(computeGraphClustersAsGraphs(NULL, structure(list(data=c(1,2,3)),
                                                            class = c("toacommunities"))),
               "Graph communities object must be specified.")
  
  expect_error(computeGraphClustersAsGraphs(NULL, structure(list(data=c(1,2,3)),
                                                            class = c("communities"))),
               "Graph communities object must be specified.")
  
  expect_error(computeGraphClustersAsGraphs(NULL, structure(list(data=c(1,2,3)),
                                                            class = c("communities", "toacommunities"))),
               "One of the arguments 'ids' or 'componentids' must be specified and contain at least one value.")
  
  expect_error(computeGraphClustersAsGraphs(NULL, structure(list(data=c(1,2,3)),
                                                            class = c("communities", "toacommunities")),
                                            ids=integer(0), componentids=character(0)),
               "One of the arguments 'ids' or 'componentids' must be specified and contain at least one value.")
  
  expect_error(computeGraphClustersAsGraphs(NULL, structure(list(data=c(1,2,3)),
                                                            class = c("communities", "toacommunities")),
                                            ids=c(1)),
               "Membership table is not defined and may not exist. Please, rerun graph clustering with createMembership=TRUE.")
  
  expect_error(computeGraphClustersAsGraphs(NULL, structure(list(data=c(1,2,3),
                                                                 membershipTableName=NULL),
                                                            class = c("communities", "toacommunities")),
                                            ids=c(1)),
               "Membership table is not defined and may not exist. Please, rerun graph clustering with createMembership=TRUE.")
  
  expect_error(computeGraphClustersAsGraphs(NULL, structure(list(data=c(1,2,3),
                                                                 membershipTableName="membership_table"),
                                                            class = c("communities", "toacommunities")),
                                            ids=c(1)),
               "Graph object must be specified.")
  
  expect_error(computeGraphClustersAsGraphs(NULL, structure(list(data=c(1,2,3),
                                                                 graph = NULL,
                                                                 membershipTableName="membership_table"),
                                                            class = c("communities", "toacommunities")),
                                            ids=c(1)),
               "Graph object must be specified.")
  
  expect_error(computeGraphClustersAsGraphs(NULL, structure(list(data=c(1,2,3),
                                                                 graph = toaGraph("vs", "es"),
                                                                 membershipTableName="membership_table"),
                                                            class = c("communities", "toacommunities")),
                                            ids=c(1), test=TRUE),
               "Must provide allTables when test==TRUE.")
  
  expect_error(computeGraphClustersAsGraphs(NULL, structure(list(data=c(1,2,3),
                                                                 graph = toaGraph("vs", "es"),
                                                                 membershipTableName="membership_table"),
                                                            class = c("communities", "toacommunities")), 
                                            ids=c(1),
                                            allTables = data.frame(TABLE_NAME=c("dallaspolice_officer_vertices",
                                                                                "dallaspolice_officer_edges_di")),
                                            test=TRUE),
               "Both vertices and edges must exist as tables or views.")
})


policeGraphUn = toaGraph(vertices="dallaspolice_officer_vertices", edges="dallaspolice_officer_edges_un", 
                         directed=FALSE, key="officer", source="officer1", target="officer2", 
                         vertexAttrnames = c("offense_count"), edgeAttrnames = c("weight"))
policeGraphDi = toaGraph(vertices="dallaspolice_officer_vertices", edges="dallaspolice_officer_edges_di", 
                         directed=TRUE, key="officer", source="officer1", target="officer2", 
                         vertexAttrnames = c("offense_count"), edgeAttrnames = c("weight"))

test_that("computeGraphClusters for connectivty works properly", {
  
  expect_equal_normalized(computeGraphClusters(conn, policeGraphUn, type="connected", 
                         distanceTableName = "public.shortestpathdistances",
                         allTables = data.frame(TABLE_NAME=c("dallaspolice_officer_vertices","dallaspolice_officer_edges_un",
                                                                                 "dallaspolice_officer_edges_di")),
                         test=TRUE),
"-- Shortest Paths Table
DROP TABLE IF EXISTS public.shortestpathdistances;
CREATE FACT TABLE public.shortestpathdistances
     DISTRIBUTE BY HASH(source) AS
     SELECT source, target, distance from AllPairsShortestPath(
       ON (SELECT officer, offense_count 
       FROM dallaspolice_officer_vertices ) AS vertices PARTITION BY officer
       ON (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_un ) AS edges PARTITION BY officer1
       TARGETKEY('officer2')
       DIRECTED('false')
     
       MAXDISTANCE('-1')
     );
--;
-- Compute connected components with properties;
SELECT ROW_NUMBER() OVER (ORDER BY vertex_count DESC, componentid) id, * 
       FROM 
         (SELECT componentid, 
                 COUNT(CASE WHEN distance=0 THEN distance ELSE NULL END) vertex_count,
                 COUNT(CASE WHEN distance=1 THEN 1 ELSE NULL END)/2  edge_count,
                 CASE WHEN SUM(distance)=0 THEN 0 
                      ELSE SUM(distance)/COUNT(CASE WHEN distance!=0 THEN distance ELSE NULL END)
                 END avg_distance,
                 MAX(distance) diameter
           FROM 
             (SELECT source, target, distance,
                     MIN(target) OVER (PARTITION BY source) componentid
                FROM 
                  (SELECT officer source, officer target, 0 distance 
                     FROM (SELECT officer 
       FROM dallaspolice_officer_vertices ) t
                    UNION 
                   SELECT source, target, distance FROM public.shortestpathdistances
                  ) t 
             ) t1
         GROUP BY 1 ) t2
    ORDER BY 1;", label="computeGraphClusters for connectivity without membership")
  
  
  expect_equal_normalized(computeGraphClusters(conn, policeGraphUn, type="connected", 
                         createMembership = TRUE, includeMembership = TRUE,
                         distanceTableName = "public.shortestpathdistances",
                         membershipTableName = "public.clustermembership",
                         allTables = data.frame(TABLE_NAME=c("dallaspolice_officer_vertices","dallaspolice_officer_edges_un",
                                                                                 "dallaspolice_officer_edges_di")),
                         test=TRUE),
"-- Shortest Paths Table
DROP TABLE IF EXISTS public.shortestpathdistances;
CREATE FACT TABLE public.shortestpathdistances
     DISTRIBUTE BY HASH(source) AS
     SELECT source, target, distance from AllPairsShortestPath(
       ON (SELECT officer, offense_count 
       FROM dallaspolice_officer_vertices ) AS vertices PARTITION BY officer
       ON (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_un ) AS edges PARTITION BY officer1
       TARGETKEY('officer2')
       DIRECTED('false')
     
       MAXDISTANCE('-1')
     );
--;
-- Compute connected components with properties;
SELECT ROW_NUMBER() OVER (ORDER BY vertex_count DESC, componentid) id, * 
       FROM 
         (SELECT componentid, 
                 COUNT(CASE WHEN distance=0 THEN distance ELSE NULL END) vertex_count,
                 COUNT(CASE WHEN distance=1 THEN 1 ELSE NULL END)/2  edge_count,
                 CASE WHEN SUM(distance)=0 THEN 0 
                      ELSE SUM(distance)/COUNT(CASE WHEN distance!=0 THEN distance ELSE NULL END)
                 END avg_distance,
                 MAX(distance) diameter
           FROM 
             (SELECT source, target, distance,
                     MIN(target) OVER (PARTITION BY source) componentid
                FROM 
                  (SELECT officer source, officer target, 0 distance 
                     FROM (SELECT officer 
       FROM dallaspolice_officer_vertices ) t
                    UNION 
                   SELECT source, target, distance FROM public.shortestpathdistances
                  ) t 
             ) t1
         GROUP BY 1 ) t2
    ORDER BY 1;
--;
-- Compute vertex membership;
DROP TABLE IF EXISTS public.clustermembership;
CREATE FACT TABLE public.clustermembership
DISTRIBUTE BY HASH(id)
AS
SELECT component.id, membership.componentid, membership.target vertex 
         FROM (SELECT ROW_NUMBER() OVER (ORDER BY vertex_count DESC, componentid) id, * 
       FROM 
         (SELECT componentid, 
                 COUNT(CASE WHEN distance=0 THEN distance ELSE NULL END) vertex_count
            FROM 
             (SELECT source, target, distance,
                     MIN(target) OVER (PARTITION BY source) componentid
                FROM 
                  (SELECT officer source, officer target, 0 distance 
                     FROM (SELECT officer 
       FROM dallaspolice_officer_vertices ) t
                    UNION 
                   SELECT source, target, distance FROM public.shortestpathdistances
                  ) t 
             ) t1
         GROUP BY 1 ) t2) component JOIN
         (SELECT source, target, distance,
                     MIN(target) OVER (PARTITION BY source) componentid
                FROM 
                  (SELECT officer source, officer target, 0 distance 
                     FROM (SELECT officer 
       FROM dallaspolice_officer_vertices ) t
                    UNION 
                   SELECT source, target, distance FROM public.shortestpathdistances
                  ) t 
             ) membership ON (component.componentid = membership.componentid)
 WHERE membership.source = membership.componentid;
SELECT * FROM public.clustermembership ORDER BY vertex;",
label="computeGraphClusters for connectivity with creating and including membership")
  
  
  expect_equal_normalized(computeGraphClusters(conn, policeGraphUn, type="connected", 
                         createMembership = TRUE, 
                         distanceTableName = "public.shortestpathdistances",
                         membershipTableName = "public.clustermembership",
                         allTables = data.frame(TABLE_NAME=c("dallaspolice_officer_vertices","dallaspolice_officer_edges_un",
                                                                                 "dallaspolice_officer_edges_di")),
                         test=TRUE),
"-- Shortest Paths Table
DROP TABLE IF EXISTS public.shortestpathdistances;
CREATE FACT TABLE public.shortestpathdistances
     DISTRIBUTE BY HASH(source) AS
     SELECT source, target, distance from AllPairsShortestPath(
       ON (SELECT officer, offense_count 
       FROM dallaspolice_officer_vertices ) AS vertices PARTITION BY officer
       ON (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_un ) AS edges PARTITION BY officer1
       TARGETKEY('officer2')
       DIRECTED('false')
     
       MAXDISTANCE('-1')
     );
--;
-- Compute connected components with properties;
SELECT ROW_NUMBER() OVER (ORDER BY vertex_count DESC, componentid) id, * 
       FROM 
         (SELECT componentid, 
                 COUNT(CASE WHEN distance=0 THEN distance ELSE NULL END) vertex_count,
                 COUNT(CASE WHEN distance=1 THEN 1 ELSE NULL END)/2  edge_count,
                 CASE WHEN SUM(distance)=0 THEN 0 
                      ELSE SUM(distance)/COUNT(CASE WHEN distance!=0 THEN distance ELSE NULL END)
                 END avg_distance,
                 MAX(distance) diameter
           FROM 
             (SELECT source, target, distance,
                     MIN(target) OVER (PARTITION BY source) componentid
                FROM 
                  (SELECT officer source, officer target, 0 distance 
                     FROM (SELECT officer 
       FROM dallaspolice_officer_vertices ) t
                    UNION 
                   SELECT source, target, distance FROM public.shortestpathdistances
                  ) t 
             ) t1
         GROUP BY 1 ) t2
    ORDER BY 1;
--;
-- Compute vertex membership;
DROP TABLE IF EXISTS public.clustermembership;
CREATE FACT TABLE public.clustermembership
DISTRIBUTE BY HASH(id)
AS
SELECT component.id, membership.componentid, membership.target vertex 
         FROM (SELECT ROW_NUMBER() OVER (ORDER BY vertex_count DESC, componentid) id, * 
       FROM 
         (SELECT componentid, 
                 COUNT(CASE WHEN distance=0 THEN distance ELSE NULL END) vertex_count
            FROM 
             (SELECT source, target, distance,
                     MIN(target) OVER (PARTITION BY source) componentid
                FROM 
                  (SELECT officer source, officer target, 0 distance 
                     FROM (SELECT officer 
       FROM dallaspolice_officer_vertices ) t
                    UNION 
                   SELECT source, target, distance FROM public.shortestpathdistances
                  ) t 
             ) t1
         GROUP BY 1 ) t2) component JOIN
         (SELECT source, target, distance,
                     MIN(target) OVER (PARTITION BY source) componentid
                FROM 
                  (SELECT officer source, officer target, 0 distance 
                     FROM (SELECT officer 
       FROM dallaspolice_officer_vertices ) t
                    UNION 
                   SELECT source, target, distance FROM public.shortestpathdistances
                  ) t 
             ) membership ON (component.componentid = membership.componentid)
 WHERE membership.source = membership.componentid;",
label="computeGraphClusters for connectivity with creating but no including membership")
  
  
  expect_equal_normalized(computeGraphClusters(conn, policeGraphUn, type="connected", 
                         distanceTableName = "public.shortestpathdistances",
                         allTables = data.frame(TABLE_NAME=c("dallaspolice_officer_vertices","dallaspolice_officer_edges_un",
                                                                                 "dallaspolice_officer_edges_di")),
                         vertexWhere = "officer ~ '[A-Z].*'", edgeWhere = "weight > 0.36", 
                         test=TRUE),
"-- Shortest Paths Table
DROP TABLE IF EXISTS public.shortestpathdistances;
CREATE FACT TABLE public.shortestpathdistances
     DISTRIBUTE BY HASH(source) AS
     SELECT source, target, distance from AllPairsShortestPath(
       ON (SELECT officer, offense_count 
       FROM dallaspolice_officer_vertices WHERE officer ~ '[A-Z].*'  ) AS vertices PARTITION BY officer
       ON (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_un WHERE (weight > 0.36) AND 
         officer1 IN (SELECT officer 
       FROM dallaspolice_officer_vertices WHERE officer ~ '[A-Z].*'  ) AND 
         officer2 IN (SELECT officer 
       FROM dallaspolice_officer_vertices WHERE officer ~ '[A-Z].*'  )  ) AS edges PARTITION BY officer1
       TARGETKEY('officer2')
       DIRECTED('false')
     
       MAXDISTANCE('-1')
     );
--;
-- Compute connected components with properties;
SELECT ROW_NUMBER() OVER (ORDER BY vertex_count DESC, componentid) id, * 
       FROM 
         (SELECT componentid, 
                 COUNT(CASE WHEN distance=0 THEN distance ELSE NULL END) vertex_count,
                 COUNT(CASE WHEN distance=1 THEN 1 ELSE NULL END)/2  edge_count,
                 CASE WHEN SUM(distance)=0 THEN 0 
                      ELSE SUM(distance)/COUNT(CASE WHEN distance!=0 THEN distance ELSE NULL END)
                 END avg_distance,
                 MAX(distance) diameter
           FROM 
             (SELECT source, target, distance,
                     MIN(target) OVER (PARTITION BY source) componentid
                FROM 
                  (SELECT officer source, officer target, 0 distance 
                     FROM (SELECT officer 
       FROM dallaspolice_officer_vertices WHERE officer ~ '[A-Z].*'  ) t
                    UNION 
                   SELECT source, target, distance FROM public.shortestpathdistances
                  ) t 
             ) t1
         GROUP BY 1 ) t2
    ORDER BY 1;",
                         label="computeGraphClusters for connectivity with vertex and edge where but without membership")
  
  
  expect_equal_normalized(computeGraphClusters(conn, policeGraphUn, type="connected", 
                         createMembership = TRUE, includeMembership = TRUE,
                         distanceTableName = "public.shortestpathdistances",
                         membershipTableName = "public.clustermembership",
                         allTables = data.frame(TABLE_NAME=c("dallaspolice_officer_vertices","dallaspolice_officer_edges_un",
                                                                                 "dallaspolice_officer_edges_di")),
                         vertexWhere = "officer ~ '[A-Z].*'", edgeWhere = "weight > 0.36", 
                         test=TRUE),
"-- Shortest Paths Table
DROP TABLE IF EXISTS public.shortestpathdistances;
CREATE FACT TABLE public.shortestpathdistances
     DISTRIBUTE BY HASH(source) AS
     SELECT source, target, distance from AllPairsShortestPath(
       ON (SELECT officer, offense_count 
       FROM dallaspolice_officer_vertices WHERE officer ~ '[A-Z].*'  ) AS vertices PARTITION BY officer
       ON (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_un WHERE (weight > 0.36) AND 
         officer1 IN (SELECT officer 
       FROM dallaspolice_officer_vertices WHERE officer ~ '[A-Z].*'  ) AND 
         officer2 IN (SELECT officer 
       FROM dallaspolice_officer_vertices WHERE officer ~ '[A-Z].*'  )  ) AS edges PARTITION BY officer1
       TARGETKEY('officer2')
       DIRECTED('false')
     
       MAXDISTANCE('-1')
     );
--;
-- Compute connected components with properties;
SELECT ROW_NUMBER() OVER (ORDER BY vertex_count DESC, componentid) id, * 
       FROM 
         (SELECT componentid, 
                 COUNT(CASE WHEN distance=0 THEN distance ELSE NULL END) vertex_count,
                 COUNT(CASE WHEN distance=1 THEN 1 ELSE NULL END)/2  edge_count,
                 CASE WHEN SUM(distance)=0 THEN 0 
                      ELSE SUM(distance)/COUNT(CASE WHEN distance!=0 THEN distance ELSE NULL END)
                 END avg_distance,
                 MAX(distance) diameter
           FROM 
             (SELECT source, target, distance,
                     MIN(target) OVER (PARTITION BY source) componentid
                FROM 
                  (SELECT officer source, officer target, 0 distance 
                     FROM (SELECT officer 
       FROM dallaspolice_officer_vertices WHERE officer ~ '[A-Z].*'  ) t
                    UNION 
                   SELECT source, target, distance FROM public.shortestpathdistances
                  ) t 
             ) t1
         GROUP BY 1 ) t2
    ORDER BY 1;
--;
-- Compute vertex membership;
DROP TABLE IF EXISTS public.clustermembership;
CREATE FACT TABLE public.clustermembership
DISTRIBUTE BY HASH(id)
AS
SELECT component.id, membership.componentid, membership.target vertex 
         FROM (SELECT ROW_NUMBER() OVER (ORDER BY vertex_count DESC, componentid) id, * 
       FROM 
         (SELECT componentid, 
                 COUNT(CASE WHEN distance=0 THEN distance ELSE NULL END) vertex_count
            FROM 
             (SELECT source, target, distance,
                     MIN(target) OVER (PARTITION BY source) componentid
                FROM 
                  (SELECT officer source, officer target, 0 distance 
                     FROM (SELECT officer 
       FROM dallaspolice_officer_vertices WHERE officer ~ '[A-Z].*'  ) t
                    UNION 
                   SELECT source, target, distance FROM public.shortestpathdistances
                  ) t 
             ) t1
         GROUP BY 1 ) t2) component JOIN
         (SELECT source, target, distance,
                     MIN(target) OVER (PARTITION BY source) componentid
                FROM 
                  (SELECT officer source, officer target, 0 distance 
                     FROM (SELECT officer 
       FROM dallaspolice_officer_vertices WHERE officer ~ '[A-Z].*'  ) t
                    UNION 
                   SELECT source, target, distance FROM public.shortestpathdistances
                  ) t 
             ) membership ON (component.componentid = membership.componentid)
 WHERE membership.source = membership.componentid;
SELECT * FROM public.clustermembership ORDER BY vertex;",
                         label="computeGraphClusters for connectivity with vertex and edge where and with membership")
  
  
  expect_equal_normalized(computeGraphClusters(conn, policeGraphUn, type="connected", 
                         createMembership = TRUE,
                         distanceTableName = "public.shortestpathdistances",
                         membershipTableName = "public.clustermembership",
                         allTables = data.frame(TABLE_NAME=c("dallaspolice_officer_vertices","dallaspolice_officer_edges_un",
                                                                                 "dallaspolice_officer_edges_di")),
                         vertexWhere = "officer ~ '[A-Z].*'", edgeWhere = "weight > 0.36", 
                         test=TRUE),
"-- Shortest Paths Table
DROP TABLE IF EXISTS public.shortestpathdistances;
CREATE FACT TABLE public.shortestpathdistances
     DISTRIBUTE BY HASH(source) AS
     SELECT source, target, distance from AllPairsShortestPath(
       ON (SELECT officer, offense_count 
       FROM dallaspolice_officer_vertices WHERE officer ~ '[A-Z].*'  ) AS vertices PARTITION BY officer
       ON (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_un WHERE (weight > 0.36) AND 
         officer1 IN (SELECT officer 
       FROM dallaspolice_officer_vertices WHERE officer ~ '[A-Z].*'  ) AND 
         officer2 IN (SELECT officer 
       FROM dallaspolice_officer_vertices WHERE officer ~ '[A-Z].*'  )  ) AS edges PARTITION BY officer1
       TARGETKEY('officer2')
       DIRECTED('false')
     
       MAXDISTANCE('-1')
     );
--;
-- Compute connected components with properties;
SELECT ROW_NUMBER() OVER (ORDER BY vertex_count DESC, componentid) id, * 
       FROM 
         (SELECT componentid, 
                 COUNT(CASE WHEN distance=0 THEN distance ELSE NULL END) vertex_count,
                 COUNT(CASE WHEN distance=1 THEN 1 ELSE NULL END)/2  edge_count,
                 CASE WHEN SUM(distance)=0 THEN 0 
                      ELSE SUM(distance)/COUNT(CASE WHEN distance!=0 THEN distance ELSE NULL END)
                 END avg_distance,
                 MAX(distance) diameter
           FROM 
             (SELECT source, target, distance,
                     MIN(target) OVER (PARTITION BY source) componentid
                FROM 
                  (SELECT officer source, officer target, 0 distance 
                     FROM (SELECT officer 
       FROM dallaspolice_officer_vertices WHERE officer ~ '[A-Z].*'  ) t
                    UNION 
                   SELECT source, target, distance FROM public.shortestpathdistances
                  ) t 
             ) t1
         GROUP BY 1 ) t2
    ORDER BY 1;
--;
-- Compute vertex membership;
DROP TABLE IF EXISTS public.clustermembership;
CREATE FACT TABLE public.clustermembership
DISTRIBUTE BY HASH(id)
AS
SELECT component.id, membership.componentid, membership.target vertex 
         FROM (SELECT ROW_NUMBER() OVER (ORDER BY vertex_count DESC, componentid) id, * 
       FROM 
         (SELECT componentid, 
                 COUNT(CASE WHEN distance=0 THEN distance ELSE NULL END) vertex_count
            FROM 
             (SELECT source, target, distance,
                     MIN(target) OVER (PARTITION BY source) componentid
                FROM 
                  (SELECT officer source, officer target, 0 distance 
                     FROM (SELECT officer 
       FROM dallaspolice_officer_vertices WHERE officer ~ '[A-Z].*'  ) t
                    UNION 
                   SELECT source, target, distance FROM public.shortestpathdistances
                  ) t 
             ) t1
         GROUP BY 1 ) t2) component JOIN
         (SELECT source, target, distance,
                     MIN(target) OVER (PARTITION BY source) componentid
                FROM 
                  (SELECT officer source, officer target, 0 distance 
                     FROM (SELECT officer 
       FROM dallaspolice_officer_vertices WHERE officer ~ '[A-Z].*'  ) t
                    UNION 
                   SELECT source, target, distance FROM public.shortestpathdistances
                  ) t 
             ) membership ON (component.componentid = membership.componentid)
 WHERE membership.source = membership.componentid;",
                         label="computeGraphClusters for connectivity with vertex and edge where and with creating but no including membership")
})
