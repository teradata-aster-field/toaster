context("computeGraphHistogram")

test_that("computeGraphHistogram throws errors", {
  
  expect_error(computeGraphHistogram(type = 'not a type'),
               ".*'arg' should be one of \"degree\", \"clustering\", \"shortestpath\", \"pagerank\", \"betweenness\", \"eigenvector\"")
  
  expect_error(computeGraphHistogram(binMethod = 'not a method'),
               ".*'arg' should be one of \"manual\", \"Sturges\", \"Scott\"")
  
  expect_error(computeGraphHistogram(NULL, "not graph"),
               "Graph object must be specified.")
  
  expect_error(computeGraphHistogram(NULL, toaGraph("vs", "es"), test=TRUE),
               "Must provide allTables when test==TRUE.")
  
  expect_error(computeGraphHistogram(NULL, toaGraph("vs", "es"), weight=TRUE, 
                                     allTables = data.frame(TABLE_NAME=c("vs","es"), stringsAsFactors = FALSE), 
                                     test=TRUE),
               "No edge attribute 'weight' found in graph.")
  
  expect_error(computeGraphHistogram(NULL, toaGraph("vs", "es"), weight="notweight",
                                     allTables = data.frame(TABLE_NAME=c("vs","es"), stringsAsFactors = FALSE), 
                                     test=TRUE),
               "No edge attribute 'notweight' found in graph.")
  
  expect_error(computeGraphHistogram(NULL, toaGraph("vs", "es"), 
                                      binMethod = 'manual', numbins = NULL,
                                      allTables = data.frame(TABLE_NAME=c("vs","es"), stringsAsFactors = FALSE), 
                                      test=TRUE),
               "Number of bins or/and at least startvalue and endvalue must be defined when method is 'manual'.")
  
  expect_error(computeGraphHistogram(NULL, toaGraph("vs", "es"), 
                                      binMethod = 'manual', startvalue = 10, endvalue = 9,
                                      allTables = data.frame(TABLE_NAME=c("vs","es"), stringsAsFactors = FALSE), 
                                      test=TRUE),
               "End value should be greater than start value.")
})

test_that("computeGraphHistogram for degree works properly", {

  expect_equal_normalized(computeGraphHistogram(NULL,
             toaGraph("graph.films_vertices", "graph.films_edges", FALSE, "name", "name1", "name2"),
             binMethod = 'manual', numbins = 30,
                      allTables = data.frame(TABLE_SCHEM=c("graph","graph"),
                                             TABLE_NAME=c("films_vertices","films_edges")),
                      test=TRUE),
"BEGIN;
--
-- Create temp table of vertex degrees
CREATE TEMP FACT TABLE graphdataforhisttemp 
     DISTRIBUTE BY HASH(key) 
     AS
     SELECT key, degree_type, degree_long degree FROM unpivot(
       ON (SELECT COALESCE(s.key, t.key) key, 
                  COALESCE(s.cnt_source,0) outdegree, 
                  COALESCE(t.cnt_target,0) indegree,  
                  COALESCE(s.cnt_source,0) + COALESCE(t.cnt_target,0) degree,
                  (COALESCE(t.cnt_target,0) + 1)/(COALESCE(s.cnt_source,0) + 1) inbyoutdegree
             FROM (SELECT name1 key, COUNT(*) cnt_source 
                     FROM (SELECT name1, name2 
         FROM graph.films_edges ) e 
                    GROUP BY 1) s FULL JOIN
                  (SELECT name2 key, COUNT(*) cnt_target 
                     FROM (SELECT name1, name2 
         FROM graph.films_edges ) e
                    GROUP BY 1) t ON (s.key = t.key)
       )
       colsToUnpivot('outdegree','indegree','degree','inbyoutdegree')
       colsToAccumulate('key')
       keepInputColumnTypes('true')
       ATTRIBUTECOLUMNNAME('degree_type')
       VALUECOLUMNNAME('degree')
     );
--
-- Degree Histogram
SELECT * FROM Hist_Reduce(
           ON Hist_Map(
             ON graphdataforhisttemp as data_input PARTITION BY ANY 
             ON hist_prep(
               ON graphdataforhisttemp VALUE_COLUMN('degree')) as data_stat DIMENSION
             BIN_SELECT('30')
             VALUE_COLUMN('degree')
             GROUP_COLUMNS('degree_type')
         ) PARTITION BY degree_type
    );
--
END", label="Degree histogram with numbins")
  
  expect_equal_normalized(computeGraphHistogram(NULL,
             toaGraph("graph.films_vertices", "graph.films_edges", FALSE, "name", "name1", "name2"),
             binMethod = 'Scott', 
             allTables = data.frame(TABLE_SCHEM=c("graph","graph"),
                                    TABLE_NAME=c("films_vertices","films_edges")),
                      test=TRUE),
"BEGIN;
--
-- Create temp table of vertex degrees
CREATE TEMP FACT TABLE graphdataforhisttemp 
     DISTRIBUTE BY HASH(key) 
     AS
     SELECT key, degree_type, degree_long degree FROM unpivot(
       ON (SELECT COALESCE(s.key, t.key) key, 
                  COALESCE(s.cnt_source,0) outdegree, 
                  COALESCE(t.cnt_target,0) indegree,  
                  COALESCE(s.cnt_source,0) + COALESCE(t.cnt_target,0) degree,
                  (COALESCE(t.cnt_target,0) + 1)/(COALESCE(s.cnt_source,0) + 1) inbyoutdegree
             FROM (SELECT name1 key, COUNT(*) cnt_source 
                     FROM (SELECT name1, name2 
         FROM graph.films_edges ) e 
                    GROUP BY 1) s FULL JOIN
                  (SELECT name2 key, COUNT(*) cnt_target 
                     FROM (SELECT name1, name2 
         FROM graph.films_edges ) e
                    GROUP BY 1) t ON (s.key = t.key)
       )
       colsToUnpivot('outdegree','indegree','degree','inbyoutdegree')
       colsToAccumulate('key')
       keepInputColumnTypes('true')
       ATTRIBUTECOLUMNNAME('degree_type')
       VALUECOLUMNNAME('degree')
     );
--
-- Degree Histogram
SELECT * FROM Hist_Reduce(
           ON Hist_Map(
             ON graphdataforhisttemp as data_input PARTITION BY ANY 
             ON hist_prep(
                 ON graphdataforhisttemp VALUE_COLUMN('degree')) as data_stat DIMENSION
             BIN_SELECT('Scott')
             VALUE_COLUMN('degree')
             GROUP_COLUMNS('degree_type')
         ) PARTITION BY degree_type
    );
--
END", label="Degree histogram with Scott method")
  
  expect_equal_normalized(computeGraphHistogram(NULL,
             toaGraph("graph.films_vertices", "graph.films_edges", FALSE, "name", "name1", "name2"),
             binMethod = 'manual', startvalue = 0, endvalue = 30, numbins = 25, 
             allTables = data.frame(TABLE_SCHEM=c("graph","graph"),
                                    TABLE_NAME=c("films_vertices","films_edges")),
                      test=TRUE),
"BEGIN;
--
-- Create temp table of vertex degrees
CREATE TEMP FACT TABLE graphdataforhisttemp 
     DISTRIBUTE BY HASH(key) 
     AS
     SELECT key, degree_type, degree_long degree FROM unpivot(
       ON (SELECT COALESCE(s.key, t.key) key, 
                  COALESCE(s.cnt_source,0) outdegree, 
                  COALESCE(t.cnt_target,0) indegree,  
                  COALESCE(s.cnt_source,0) + COALESCE(t.cnt_target,0) degree,
                  (COALESCE(t.cnt_target,0) + 1)/(COALESCE(s.cnt_source,0) + 1) inbyoutdegree
             FROM (SELECT name1 key, COUNT(*) cnt_source 
                     FROM (SELECT name1, name2 
         FROM graph.films_edges ) e 
                    GROUP BY 1) s FULL JOIN
                  (SELECT name2 key, COUNT(*) cnt_target 
                     FROM (SELECT name1, name2 
         FROM graph.films_edges ) e
                    GROUP BY 1) t ON (s.key = t.key)
       )
       colsToUnpivot('outdegree','indegree','degree','inbyoutdegree')
       colsToAccumulate('key')
       keepInputColumnTypes('true')
       ATTRIBUTECOLUMNNAME('degree_type')
       VALUECOLUMNNAME('degree')
     );
--
-- Degree Histogram
SELECT * FROM Hist_Reduce(
           ON Hist_Map(
             ON graphdataforhisttemp as data_input PARTITION BY ANY 
          binsize('1.2')
                         startvalue('0')
                         endvalue('30')
             VALUE_COLUMN('degree')
             GROUP_COLUMNS('degree_type')
         ) PARTITION BY degree_type
    );
--
END", label="Degree histogram with start value, end value, and numbins")
  
})


airlineGraphGlobal = toaGraph("karthik.global_vertex_view", "karthik.global_edges_view", TRUE, 
                              "city", "vertex1", "vertex2",
                              edgeAttrnames = c("score"))
policeGraphUn = toaGraph("dallaspolice_officer_vertices", "dallaspolice_officer_edges_un", FALSE,
                         "officer", "officer1", "officer2", vertexAttrnames = c("offense_count"),
                         edgeAttrnames = c("weight"))

test_that("computeGraphHistogram for clustering works properly", {
  
  expect_equal_normalized(computeGraphHistogram(conn, airlineGraphGlobal, type='clustering',
                           binMethod = 'manual', binsize = 0.01, endvalue = 1,
                           allTables = data.frame(TABLE_SCHEM=c("karthik","karthik"),
                                                  TABLE_NAME=c("global_vertex_view","global_edges_view")),
                           test=TRUE),
"BEGIN;
--
-- Create temp table of vertex degrees
CREATE TEMP FACT TABLE graphdataforhisttemp 
     DISTRIBUTE BY HASH(key) 
     AS
     SELECT city key, cc_type, coalesce(cc_double, cc_str::double) cc FROM unpivot(
       ON (SELECT * FROM LocalClusteringCoefficient(
             ON (SELECT city 
       FROM karthik.global_vertex_view ) AS vertices PARTITION BY city
             ON (SELECT vertex1, vertex2, score 
         FROM karthik.global_edges_view ) AS edges PARTITION BY vertex1
             targetKey('vertex2')
           
             directed('true')
             accumulate('city')
       ))
       colsToUnpivot('cyc_cc','mid_cc','in_cc','out_cc','avg_cc')
       colsToAccumulate('city')
       keepInputColumnTypes('true')
       ATTRIBUTECOLUMNNAME('cc_type')
       VALUECOLUMNNAME('cc')
     );
--
-- Degree Histogram
SELECT * FROM Hist_Reduce(
           ON Hist_Map(
             ON graphdataforhisttemp as data_input PARTITION BY ANY 
          binsize('0.01')
                         startvalue('0')
                         endvalue('1')
             VALUE_COLUMN('cc')
            GROUP_COLUMNS('cc_type')
         ) PARTITION BY cc_type
    );
--
END", label="Clustering histogram with manual binmethod, binsize, endvalue")
  
  expect_equal_normalized(computeGraphHistogram(NULL, policeGraphUn, type='clustering', numbins = 100,
                          allTables = data.frame(TABLE_NAME=c("dallaspolice_officer_vertices","dallaspolice_officer_edges_un")),
                          test=TRUE),
"BEGIN;
--
-- Create temp table of vertex degrees
CREATE TEMP FACT TABLE graphdataforhisttemp 
     DISTRIBUTE BY HASH(key) 
     AS
     SELECT officer key, cc_type, coalesce(cc_double, cc_str::double) cc FROM unpivot(
       ON (SELECT * FROM LocalClusteringCoefficient(
             ON (SELECT officer, offense_count 
       FROM dallaspolice_officer_vertices ) AS vertices PARTITION BY officer
             ON (SELECT officer1, officer2, weight 
         FROM dallaspolice_officer_edges_un ) AS edges PARTITION BY officer1
             targetKey('officer2')
           
             directed('false')
             accumulate('officer')
       ))
       colsToUnpivot('cc')
       colsToAccumulate('officer')
       keepInputColumnTypes('true')
       ATTRIBUTECOLUMNNAME('cc_type')
       VALUECOLUMNNAME('cc')
     );
--
-- Degree Histogram
SELECT * FROM Hist_Reduce(
           ON Hist_Map(
             ON graphdataforhisttemp as data_input PARTITION BY ANY 
          ON hist_prep(
                           ON graphdataforhisttemp VALUE_COLUMN('cc')) as data_stat DIMENSION
                           BIN_SELECT('100')
             VALUE_COLUMN('cc')
            GROUP_COLUMNS('cc_type')
         ) PARTITION BY cc_type
    );
--
END", label="Clustering histogram with numbins (manual by default)")
  
})