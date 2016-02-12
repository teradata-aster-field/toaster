context("computeGraph")

test_that("toaGraph throws errors", {
  
  expect_error(toaGraph(NULL, NULL),
               "Both vertices and edges must be defined.")
  
  expect_error(toaGraph("vertices", NULL),
               "Both vertices and edges must be defined.")
  
  expect_error(toaGraph(NULL, "edges"),
               "Both vertices and edges must be defined.")
  
  
})

test_that("computeGraph throws errors", {
  
  expect_error(computeGraph(NULL, "not graph"),
               "Graph object must be specified.")
  
  expect_error(computeGraph(NULL, toaGraph("vs", "es"), test=TRUE),
               "Must provide allTables when test==TRUE.")
  
  expect_error(computeGraph(NULL, toaGraph("vs", "es"), v=logical(1), 
                            allTables = data.frame(TABLE_NAME=c("vs","es"), stringsAsFactors = FALSE), test=TRUE),
               ".*Values must be either numeric or character only.")
})


simplestGraph = toaGraph("vertices", "edges")
simplestGraphWithEdgeAttrs = toaGraph("vertices", "edges", edgeAttrnames = c("weight","cost"))
vertexWhereGraph = toaGraph("vertices", "edges", vertexWhere = "state='TX'")

test_that("computeGraph works properly", {
  
  expect_equal_normalized(computeGraph(NULL, simplestGraph, 
                                       allTables = data.frame(TABLE_NAME=c("vertices","edges"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                           SELECT source, target FROM edges ")
  
  expect_equal_normalized(computeGraph(NULL, simplestGraph, v=character(0),
                                       allTables = data.frame(TABLE_NAME=c("vertices","edges"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                           SELECT source, target FROM edges ")
  
  expect_equal_normalized(computeGraph(NULL, simplestGraph, v=list(),
                                       allTables = data.frame(TABLE_NAME=c("vertices","edges"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                           SELECT source, target FROM edges ")
  
  expect_equal_normalized(computeGraph(NULL, simplestGraphWithEdgeAttrs,
                                       allTables = data.frame(TABLE_NAME=c("vertices","edges"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                           SELECT source, target, weight, cost FROM edges ")
  
  expect_equal_normalized(computeGraph(NULL, vertexWhereGraph,
                                       allTables = data.frame(TABLE_NAME=c("vertices","edges"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                            SELECT source, target FROM edges 
                            WHERE source IN (SELECT id FROM vertices WHERE state='TX' )
                              AND target IN (SELECT id FROM vertices WHERE state='TX' ) ")
  
  expect_equal_normalized(computeGraph(NULL, simplestGraph, v=list(1,2,3),
                                       allTables = data.frame(TABLE_NAME=c("vertices","edges"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                            SELECT source, target FROM edges 
                            WHERE source IN (SELECT id FROM vertices WHERE id IN (1, 2, 3) )
                              AND target IN (SELECT id FROM vertices WHERE id IN (1, 2, 3) ) ")
  
  expect_equal_normalized(computeGraph(NULL, simplestGraph, v=c(1,2,3),
                                       allTables = data.frame(TABLE_NAME=c("vertices","edges"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                            SELECT source, target FROM edges 
                            WHERE source IN (SELECT id FROM vertices WHERE id IN (1, 2, 3) )
                              AND target IN (SELECT id FROM vertices WHERE id IN (1, 2, 3) ) ")
  
  expect_equal_normalized(computeGraph(NULL, simplestGraph, v=list('a','b','c'),
                                       allTables = data.frame(TABLE_NAME=c("vertices","edges"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                            SELECT source, target FROM edges 
                            WHERE source IN (SELECT id FROM vertices WHERE id IN ('a', 'b', 'c') )
                              AND target IN (SELECT id FROM vertices WHERE id IN ('a', 'b', 'c') ) ")
  
  expect_equal_normalized(computeGraph(NULL, 
             toaGraph("graph.films_vertices", "graph.films_edges", FALSE, "name", "name1", "name2",
                      vertexAttrnames = "role", 
                      edgeAttrnames = c("weight","years")),
                      allTables = data.frame(TABLE_SCHEM=c("graph","graph"),
                                             TABLE_NAME=c("films_vertices","films_edges")),
                      test=TRUE),
             "-- Edges Select
             SELECT name1, name2, weight, years 
               FROM graph.films_edges ;
             --
             -- Vertices Select
             SELECT name, role FROM graph.films_vertices ")
  
  expect_equal_normalized(computeGraph(NULL,
                                       toaGraph("graph.films_vertices", "graph.films_edges", FALSE,
                                                "name", "name1", "name2", 
                                                vertexAttrnames = "role",
                                                vertexWhere = "role = 'Actor'"),
                                                allTables = data.frame(TABLE_SCHEM=c("graph","graph"),
                                             TABLE_NAME=c("films_vertices","films_edges")),
                      test=TRUE),
                      
                      "-- Edges Select
                      SELECT name1, name2 
                        FROM graph.films_edges
                       WHERE name1 IN (SELECT name FROM graph.films_vertices WHERE role = 'Actor' )
                         AND name2 IN (SELECT name FROM graph.films_vertices WHERE role = 'Actor' ) ;
                      --
                      -- Vertices Select
                      SELECT name, role FROM graph.films_vertices
                       WHERE role = 'Actor' ")
  
  expect_equal_normalized(computeGraph(NULL,
                                       toaGraph("graph.films_vertices", "graph.films_edges", FALSE,
                                                "name", "name1", "name2",
                                                vertexAttrnames = "role",
                                                vertexWhere = "role = 'Actor'"),
                                       v = c("Cruz", "Pacino", "Beluci", "Portman"),
                                       allTables = data.frame(TABLE_SCHEM=c("graph","graph"),
                                                              TABLE_NAME=c("films_vertices","films_edges")),
                      test=TRUE),
                      
                      "-- Edges Select
                      SELECT name1, name2 
                        FROM graph.films_edges
                       WHERE name1 IN (SELECT name FROM graph.films_vertices WHERE (role = 'Actor') AND 
                                                                                    name IN ('Cruz', 'Pacino', 'Beluci', 'Portman') )
                         AND name2 IN (SELECT name FROM graph.films_vertices WHERE (role = 'Actor') AND
                                                                                    name IN ('Cruz', 'Pacino', 'Beluci', 'Portman') ) ;
                      --
                      -- Vertices Select
                      SELECT name, role FROM graph.films_vertices
                       WHERE (role = 'Actor') AND name IN ('Cruz', 'Pacino', 'Beluci', 'Portman') ")
  
  expect_equal_normalized(computeGraph(NULL,
                                       toaGraph("graph.films_vertices", "graph.films_edges", FALSE,
                                                "name", "name1", "name2",
                                                vertexAttrnames = "role",
                                                vertexWhere = "role = 'Actor'"),
                                       edgeWhere = "years > 0",
                                       v = c("Cruz", "Pacino", "Beluci", "Portman"),
                                       allTables = data.frame(TABLE_SCHEM=c("graph","graph"),
                                                              TABLE_NAME=c("films_vertices","films_edges")),
                      test=TRUE),
                      
                      "-- Edges Select
                      SELECT name1, name2 
                        FROM graph.films_edges
                       WHERE (years > 0) 
                         AND name1 IN (SELECT name FROM graph.films_vertices WHERE (role = 'Actor') AND 
                                                                                    name IN ('Cruz', 'Pacino', 'Beluci', 'Portman') )
                         AND name2 IN (SELECT name FROM graph.films_vertices WHERE (role = 'Actor') AND
                                                                                    name IN ('Cruz', 'Pacino', 'Beluci', 'Portman') ) ;
                      --
                      -- Vertices Select
                      SELECT name, role FROM graph.films_vertices
                       WHERE (role = 'Actor') AND name IN ('Cruz', 'Pacino', 'Beluci', 'Portman') ")
  
})


test_that("computeEgoGraph works properly", {
  
  expect_equal_normalized(computeEgoGraph(NULL, 
                                          toaGraph("graph.films_vertices", "graph.films_edges", FALSE,
                                                   "name", "name1", "name2"), 
                                          ego='Keanu Reeves', createDistanceAttr = FALSE,
                                          allTables = data.frame(TABLE_SCHEM=c("graph","graph"),
                                                              TABLE_NAME=c("films_vertices","films_edges")),
                                          test=TRUE),
"BEGIN;
--
-- Create temp table of the shortest paths from ego vertices
CREATE TEMP FACT TABLE egographtemp 
       DISTRIBUTE BY HASH(source) 
       AS
       SELECT source, target, distance FROM AllPairsShortestPath(
         ON (SELECT name 
       FROM graph.films_vertices ) AS vertices PARTITION BY name
         ON (SELECT name1, name2 
         FROM graph.films_edges ) AS edges PARTITION BY name1
         ON (SELECT name 
       FROM graph.films_vertices WHERE name IN ('Keanu Reeves')  ) AS sources PARTITION BY name
         TARGETKEY('name2')
         DIRECTED('false')
         MAXDISTANCE('1')
       );
--
-- Edges Select
SELECT e.*
         FROM (SELECT name1, name2 
         FROM graph.films_edges ) e 
        WHERE name1 IN (SELECT target FROM egographtemp WHERE source = 'Keanu Reeves')
          AND name2 IN (SELECT target FROM egographtemp WHERE source = 'Keanu Reeves')
       UNION
       SELECT e.*
         FROM (SELECT name1, name2 
         FROM graph.films_edges ) e
        WHERE name1 = 'Keanu Reeves'
           OR name2 = 'Keanu Reeves';
--
END")
  
  expect_equal_normalized(computeEgoGraph(NULL, 
                                          toaGraph("graph.films_vertices", "graph.films_edges", FALSE,
                                                   "name", "name1", "name2"), 
                                          ego='Keanu Reeves',
                                          allTables = data.frame(TABLE_SCHEM=c("graph","graph"),
                                                              TABLE_NAME=c("films_vertices","films_edges")),
                                          test=TRUE),
"BEGIN;
--
-- Create temp table of the shortest paths from ego vertices
CREATE TEMP FACT TABLE egographtemp 
       DISTRIBUTE BY HASH(source) 
       AS
       SELECT source, target, distance FROM AllPairsShortestPath(
         ON (SELECT name 
       FROM graph.films_vertices ) AS vertices PARTITION BY name
         ON (SELECT name1, name2 
         FROM graph.films_edges ) AS edges PARTITION BY name1
         ON (SELECT name 
       FROM graph.films_vertices WHERE name IN ('Keanu Reeves')  ) AS sources PARTITION BY name
         TARGETKEY('name2')
         DIRECTED('false')
         MAXDISTANCE('1')
       );
--
-- Edges Select
SELECT e.*
         FROM (SELECT name1, name2 
         FROM graph.films_edges ) e 
        WHERE name1 IN (SELECT target FROM egographtemp WHERE source = 'Keanu Reeves')
          AND name2 IN (SELECT target FROM egographtemp WHERE source = 'Keanu Reeves')
       UNION
       SELECT e.*
         FROM (SELECT name1, name2 
         FROM graph.films_edges ) e
        WHERE name1 = 'Keanu Reeves'
           OR name2 = 'Keanu Reeves';
--
-- Vertices Select
SELECT v.*, eg.distance __distance_attr__ 
           FROM egographtemp eg JOIN
                (SELECT name 
       FROM graph.films_vertices ) v ON (eg.target = v.name)
          WHERE eg.source = 'Keanu Reeves'
         UNION
         SELECT v.*, 0 __distance_attr__ 
           FROM egographtemp eg JOIN
                (SELECT name 
       FROM graph.films_vertices ) v ON (eg.source = v.name)
          WHERE eg.source = 'Keanu Reeves';
--
END")
  
  expect_equal_normalized(computeEgoGraph(conn, 
                                          toaGraph("graph.films_vertices", "graph.films_edges", FALSE,
                                                   "name", "name1", "name2",
                                                   vertexAttrnames = "role", edgeAttrnames = c("weight","years")),
                ego = c('Keanu Reeves','Takeshi Kitano'), order = 2,
                test=TRUE, allTables = data.frame(TABLE_SCHEM=c("graph","graph"),
                                               TABLE_NAME=c("films_vertices","films_edges"))),
"BEGIN;
--
-- Create temp table of the shortest paths from ego vertices
CREATE TEMP FACT TABLE egographtemp 
       DISTRIBUTE BY HASH(source) 
       AS
       SELECT source, target, distance FROM AllPairsShortestPath(
         ON (SELECT name, role 
       FROM graph.films_vertices ) AS vertices PARTITION BY name
         ON (SELECT name1, name2, weight, years 
         FROM graph.films_edges ) AS edges PARTITION BY name1
         ON (SELECT name, role 
       FROM graph.films_vertices WHERE name IN ('Keanu Reeves', 'Takeshi Kitano')  ) AS sources PARTITION BY name
         TARGETKEY('name2')
         DIRECTED('false')
         MAXDISTANCE('2')
       );
--
-- Edges Select
SELECT e.*
         FROM (SELECT name1, name2, weight, years 
         FROM graph.films_edges ) e 
        WHERE name1 IN (SELECT target FROM egographtemp WHERE source = 'Keanu Reeves')
          AND name2 IN (SELECT target FROM egographtemp WHERE source = 'Keanu Reeves')
       UNION
       SELECT e.*
         FROM (SELECT name1, name2, weight, years 
         FROM graph.films_edges ) e
        WHERE name1 = 'Keanu Reeves'
           OR name2 = 'Keanu Reeves';
--
-- Vertices Select
SELECT v.*, eg.distance __distance_attr__ 
           FROM egographtemp eg JOIN
                (SELECT name, role 
       FROM graph.films_vertices ) v ON (eg.target = v.name)
          WHERE eg.source = 'Keanu Reeves'
         UNION
         SELECT v.*, 0 __distance_attr__ 
           FROM egographtemp eg JOIN
                (SELECT name, role 
       FROM graph.films_vertices ) v ON (eg.source = v.name)
          WHERE eg.source = 'Keanu Reeves';
--
-- Edges Select
SELECT e.*
         FROM (SELECT name1, name2, weight, years 
         FROM graph.films_edges ) e 
        WHERE name1 IN (SELECT target FROM egographtemp WHERE source = 'Takeshi Kitano')
          AND name2 IN (SELECT target FROM egographtemp WHERE source = 'Takeshi Kitano')
       UNION
       SELECT e.*
         FROM (SELECT name1, name2, weight, years 
         FROM graph.films_edges ) e
        WHERE name1 = 'Takeshi Kitano'
           OR name2 = 'Takeshi Kitano';
--
-- Vertices Select
SELECT v.*, eg.distance __distance_attr__ 
           FROM egographtemp eg JOIN
                (SELECT name, role 
       FROM graph.films_vertices ) v ON (eg.target = v.name)
          WHERE eg.source = 'Takeshi Kitano'
         UNION
         SELECT v.*, 0 __distance_attr__ 
           FROM egographtemp eg JOIN
                (SELECT name, role 
       FROM graph.films_vertices ) v ON (eg.source = v.name)
          WHERE eg.source = 'Takeshi Kitano';
--
END")
  
})