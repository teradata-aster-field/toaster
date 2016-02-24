context("computeGraphHistograms")

test_that("computeGraphHistograms throws errors", {
  
  expect_error(computeGraphHistograms(binMethod = 'not a method'),
               ".*'arg' should be one of \"manual\", \"Sturges\", \"Scott\"")
  
  expect_error(computeGraphHistograms(NULL, "not graph"),
               "Graph object must be specified.")
  
  expect_error(computeGraphHistograms(NULL, toaGraph("vs", "es"), test=TRUE),
               "Must provide allTables when test==TRUE.")
  
  expect_error(computeGraphHistograms(NULL, toaGraph("vs", "es"), 
                                      binMethod = 'manual', numbins = NULL,
                                      allTables = data.frame(TABLE_NAME=c("vs","es"), stringsAsFactors = FALSE), 
                                      test=TRUE),
               "Number of bins or/and at least startvalue and endvalue must be defined when method is 'manual'.")
  
  expect_error(computeGraphHistograms(NULL, toaGraph("vs", "es"), 
                                      binMethod = 'manual', startvalue = 10, endvalue = 9,
                                      allTables = data.frame(TABLE_NAME=c("vs","es"), stringsAsFactors = FALSE), 
                                      test=TRUE),
               "End value should be greater than start value.")
  
})

test_that("computeGraphHistograms works properly", {

  expect_equal_normalized(computeGraphHistograms(NULL,
             toaGraph("graph.films_vertices", "graph.films_edges", FALSE, "name", "name1", "name2"),
             binMethod = 'manual', numbins = 30,
                      allTables = data.frame(TABLE_SCHEM=c("graph","graph"),
                                             TABLE_NAME=c("films_vertices","films_edges")),
                      test=TRUE),
"BEGIN;
--
-- Create temp table of vertex degrees
CREATE TEMP FACT TABLE degreegraphtemp 
     DISTRIBUTE BY HASH(key) 
     AS
     SELECT key, degree_type, degree_long degree FROM unpivot(
       ON (SELECT COALESCE(s.key, t.key) key, 
                  COALESCE(s.cnt_source,0) outdegree, 
                  COALESCE(t.cnt_target,0) indegree,  
                  COALESCE(s.cnt_source,0) + COALESCE(t.cnt_target,0) degree
             FROM (SELECT name1 key, COUNT(*) cnt_source 
                     FROM (SELECT name1, name2 
         FROM graph.films_edges ) e 
                    GROUP BY 1) s FULL JOIN
                  (SELECT name2 key, COUNT(*) cnt_target 
                     FROM (SELECT name1, name2 
         FROM graph.films_edges ) e
                    GROUP BY 1) t ON (s.key = t.key)
       )
       colsToUnpivot('outdegree','indegree','degree')
       colsToAccumulate('key')
       keepInputColumnTypes('true')
       ATTRIBUTECOLUMNNAME('degree_type')
       VALUECOLUMNNAME('degree')
     );
--
-- Degree Histogram
SELECT * FROM Hist_Reduce(
           ON Hist_Map(
             ON degreegraphtemp as data_input PARTITION BY ANY 
             ON hist_prep(
               ON degreegraphtemp VALUE_COLUMN('degree')) as data_stat DIMENSION
             BIN_SELECT('30')
             VALUE_COLUMN('degree')
             GROUP_COLUMNS('degree_type')
         ) PARTITION BY degree_type
    );
--
END")
  
  expect_equal_normalized(computeGraphHistograms(NULL,
             toaGraph("graph.films_vertices", "graph.films_edges", FALSE, "name", "name1", "name2"),
             binMethod = 'Scott', 
             allTables = data.frame(TABLE_SCHEM=c("graph","graph"),
                                    TABLE_NAME=c("films_vertices","films_edges")),
                      test=TRUE),
"BEGIN;
--
-- Create temp table of vertex degrees
CREATE TEMP FACT TABLE degreegraphtemp 
     DISTRIBUTE BY HASH(key) 
     AS
     SELECT key, degree_type, degree_long degree FROM unpivot(
       ON (SELECT COALESCE(s.key, t.key) key, 
                  COALESCE(s.cnt_source,0) outdegree, 
                  COALESCE(t.cnt_target,0) indegree,  
                  COALESCE(s.cnt_source,0) + COALESCE(t.cnt_target,0) degree
             FROM (SELECT name1 key, COUNT(*) cnt_source 
                     FROM (SELECT name1, name2 
         FROM graph.films_edges ) e 
                    GROUP BY 1) s FULL JOIN
                  (SELECT name2 key, COUNT(*) cnt_target 
                     FROM (SELECT name1, name2 
         FROM graph.films_edges ) e
                    GROUP BY 1) t ON (s.key = t.key)
       )
       colsToUnpivot('outdegree','indegree','degree')
       colsToAccumulate('key')
       keepInputColumnTypes('true')
       ATTRIBUTECOLUMNNAME('degree_type')
       VALUECOLUMNNAME('degree')
     );
--
-- Degree Histogram
SELECT * FROM Hist_Reduce(
           ON Hist_Map(
             ON degreegraphtemp as data_input PARTITION BY ANY 
             ON hist_prep(
                 ON degreegraphtemp VALUE_COLUMN('degree')) as data_stat DIMENSION
             BIN_SELECT('Scott')
             VALUE_COLUMN('degree')
             GROUP_COLUMNS('degree_type')
         ) PARTITION BY degree_type
    );
--
END")
  
  expect_equal_normalized(computeGraphHistograms(NULL,
             toaGraph("graph.films_vertices", "graph.films_edges", FALSE, "name", "name1", "name2"),
             binMethod = 'manual', startvalue = 0, endvalue = 30, numbins = 25, 
             allTables = data.frame(TABLE_SCHEM=c("graph","graph"),
                                    TABLE_NAME=c("films_vertices","films_edges")),
                      test=TRUE),
"BEGIN;
--
-- Create temp table of vertex degrees
CREATE TEMP FACT TABLE degreegraphtemp 
     DISTRIBUTE BY HASH(key) 
     AS
     SELECT key, degree_type, degree_long degree FROM unpivot(
       ON (SELECT COALESCE(s.key, t.key) key, 
                  COALESCE(s.cnt_source,0) outdegree, 
                  COALESCE(t.cnt_target,0) indegree,  
                  COALESCE(s.cnt_source,0) + COALESCE(t.cnt_target,0) degree
             FROM (SELECT name1 key, COUNT(*) cnt_source 
                     FROM (SELECT name1, name2 
         FROM graph.films_edges ) e 
                    GROUP BY 1) s FULL JOIN
                  (SELECT name2 key, COUNT(*) cnt_target 
                     FROM (SELECT name1, name2 
         FROM graph.films_edges ) e
                    GROUP BY 1) t ON (s.key = t.key)
       )
       colsToUnpivot('outdegree','indegree','degree')
       colsToAccumulate('key')
       keepInputColumnTypes('true')
       ATTRIBUTECOLUMNNAME('degree_type')
       VALUECOLUMNNAME('degree')
     );
--
-- Degree Histogram
SELECT * FROM Hist_Reduce(
           ON Hist_Map(
             ON degreegraphtemp as data_input PARTITION BY ANY 
          binsize('1.2')
                         startvalue('0')
                         endvalue('30')
             VALUE_COLUMN('degree')
             GROUP_COLUMNS('degree_type')
         ) PARTITION BY degree_type
    );
--
END")
  
})