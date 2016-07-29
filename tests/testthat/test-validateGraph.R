context("validateGraph")

test_that("validateGraph throws errors", {
  
  expect_error(validateGraph(NULL, "not graph"),
               "Graph object must be specified.")
  
  expect_error(validateGraph(NULL, toaGraph("vs", "es"), test=TRUE),
               "Must provide allTables when test==TRUE.")
  
  expect_error(validateGraph(NULL, toaGraph("vs", "es"), v=list(logical(1)), 
                            allTables = data.frame(TABLE_NAME=c("vs","_es_"), stringsAsFactors = FALSE), test=TRUE),
               "Both vertices and edges must exist as tables or views.")
  
  expect_error(validateGraph(NULL, toaGraph("vs", "es"), weight="notweight",
                                     allTables = data.frame(TABLE_NAME=c("vs","es"), stringsAsFactors = FALSE), 
                                     test=TRUE),
               "No edge attribute 'notweight' found in graph.")
  
})


policeGraphUn = toaGraph("dallaspolice_officer_vertices", "dallaspolice_officer_edges_un", FALSE,
                         "officer", "officer1", "officer2", vertexAttrnames = c("offense_count"),
                         edgeAttrnames = c("weight"))
policeGraphDi = toaGraph("dallaspolice_officer_vertices", "dallaspolice_officer_edges_di", TRUE,
                         "officer", "officer1", "officer2", vertexAttrnames = c("offense_count"),
                         edgeAttrnames = c("weight"))


test_that("validateGraph works properly", {
  
  expect_equal_normalized(validateGraph(NULL, policeGraphUn, 
                                        allTables = data.frame(TABLE_NAME=c("dallaspolice_officer_vertices",
                                                                            "dallaspolice_officer_edges_un",
                                                                            "dallaspolice_officer_edges_di")),
                                        test=TRUE), 
"-- Check non-existent source vertices
SELECT COUNT(*) FROM (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_un ) es WHERE officer1 NOT IN 
       (SELECT officer FROM (SELECT officer 
       FROM dallaspolice_officer_vertices ) v );
--
-- Check non-existent target vertices
SELECT COUNT(*) FROM (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_un ) es WHERE officer2 NOT IN 
       (SELECT officer FROM (SELECT officer 
       FROM dallaspolice_officer_vertices ) v );
--
-- Check duplicate vertices
SELECT COUNT(*) FROM (SELECT officer 
         FROM (SELECT officer 
       FROM dallaspolice_officer_vertices ) vs 
        GROUP BY 1 HAVING COUNT(*) > 1) t;
--
-- Check duplicate edges
SELECT COUNT(*) FROM (
         SELECT source, target 
           FROM (SELECT officer1 source, officer2 target FROM (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_un ) es1 
                  UNION ALL
                 SELECT officer2 source, officer1 target FROM (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_un ) es2
                ) t
          GROUP BY 1,2 
         HAVING COUNT(*) > 1
      ) t2;
--
-- Check self-loops
SELECT COUNT(*) 
       FROM (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_un ) es 
      WHERE officer1 = officer2;", label="Undirected graph")
  
  
  expect_equal_normalized(validateGraph(NULL, policeGraphUn, weight=TRUE,
                                        allTables = data.frame(TABLE_NAME=c("dallaspolice_officer_vertices",
                                                                            "dallaspolice_officer_edges_un",
                                                                            "dallaspolice_officer_edges_di")),
                                        test=TRUE), 
"-- Check non-existent source vertices
SELECT COUNT(*) FROM (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_un ) es WHERE officer1 NOT IN 
       (SELECT officer FROM (SELECT officer 
       FROM dallaspolice_officer_vertices ) v );
--
-- Check non-existent target vertices
SELECT COUNT(*) FROM (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_un ) es WHERE officer2 NOT IN 
       (SELECT officer FROM (SELECT officer 
       FROM dallaspolice_officer_vertices ) v );
--
-- Check duplicate vertices
SELECT COUNT(*) FROM (SELECT officer 
         FROM (SELECT officer 
       FROM dallaspolice_officer_vertices ) vs 
        GROUP BY 1 HAVING COUNT(*) > 1) t;
--
-- Check duplicate edges
SELECT COUNT(*) FROM (
         SELECT source, target 
           FROM (SELECT officer1 source, officer2 target FROM (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_un ) es1 
                  UNION ALL
                 SELECT officer2 source, officer1 target FROM (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_un ) es2
                ) t
          GROUP BY 1,2 
         HAVING COUNT(*) > 1
      ) t2;
--
-- Check self-loops
SELECT COUNT(*) 
       FROM (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_un ) es 
      WHERE officer1 = officer2;
--
-- Check non-negative weight
SELECT COUNT(*)
       FROM (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_un ) es 
      WHERE weight <= 0;", label="Undirected graph with weight=TRUE")
  
  
  expect_equal_normalized(validateGraph(NULL, policeGraphDi,
                                        allTables = data.frame(TABLE_NAME=c("dallaspolice_officer_vertices",
                                                                            "dallaspolice_officer_edges_un",
                                                                            "dallaspolice_officer_edges_di")),
                                        test=TRUE),
"-- Check non-existent source vertices
SELECT COUNT(*) FROM (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_di ) es WHERE officer1 NOT IN 
       (SELECT officer FROM (SELECT officer 
       FROM dallaspolice_officer_vertices ) v );
--
-- Check non-existent target vertices
SELECT COUNT(*) FROM (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_di ) es WHERE officer2 NOT IN 
       (SELECT officer FROM (SELECT officer 
       FROM dallaspolice_officer_vertices ) v );
--
-- Check duplicate vertices
SELECT COUNT(*) FROM (SELECT officer 
         FROM (SELECT officer 
       FROM dallaspolice_officer_vertices ) vs 
        GROUP BY 1 HAVING COUNT(*) > 1) t;
--
-- Check duplicate edges
SELECT COUNT(*) FROM (
         SELECT officer1, officer2 
           FROM (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_di ) es 
          GROUP BY 1,2 
         HAVING COUNT(*) > 1
       ) t2;
--
-- Check self-loops
SELECT COUNT(*) 
       FROM (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_di ) es 
      WHERE officer1 = officer2;", label="Directed graph")
  
  expect_equal_normalized(validateGraph(NULL, policeGraphDi, weight='weight',
                                        allTables = data.frame(TABLE_NAME=c("dallaspolice_officer_vertices",
                                                                            "dallaspolice_officer_edges_un",
                                                                            "dallaspolice_officer_edges_di")),
                                        test=TRUE),
"-- Check non-existent source vertices
SELECT COUNT(*) FROM (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_di ) es WHERE officer1 NOT IN 
       (SELECT officer FROM (SELECT officer 
       FROM dallaspolice_officer_vertices ) v );
--
-- Check non-existent target vertices
SELECT COUNT(*) FROM (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_di ) es WHERE officer2 NOT IN 
       (SELECT officer FROM (SELECT officer 
       FROM dallaspolice_officer_vertices ) v );
--
-- Check duplicate vertices
SELECT COUNT(*) FROM (SELECT officer 
         FROM (SELECT officer 
       FROM dallaspolice_officer_vertices ) vs 
        GROUP BY 1 HAVING COUNT(*) > 1) t;
--
-- Check duplicate edges
SELECT COUNT(*) FROM (
         SELECT officer1, officer2 
           FROM (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_di ) es 
          GROUP BY 1,2 
         HAVING COUNT(*) > 1
       ) t2;
--
-- Check self-loops
SELECT COUNT(*) 
       FROM (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_di ) es 
      WHERE officer1 = officer2;
--
-- Check non-negative weight
SELECT COUNT(*)
       FROM (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_di ) es 
      WHERE weight <= 0;", label="Directed graph with weight='weight'")
  
})