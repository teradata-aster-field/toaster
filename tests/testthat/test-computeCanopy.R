context("computeCanopy")

batting_info = dget("_battingInfo.dat")

test_that("computeCanopy throws errors", {
  
  expect_error(computeCanopy(NULL),
               'argument "tightDistance" is missing, with no default')
  
  expect_error(computeCanopy(NULL, tightDistance = 0),
               'argument "looseDistance" is missing, with no default')
  
  expect_error(computeCanopy(NULL, tightDistance = 0, looseDistance = 1),
               "Connection is not valid RODBC object.")
  
  expect_error(computeCanopy(NULL, tableName="fielding", test=TRUE),
               "Must provide tableInfo when test==TRUE")
  
  expect_error(computeCanopy(NULL, tightDistance = 0.1, looseDistance = 0.1, tableInfo = batting_info, test = TRUE),
               "The loose distance must be greater than the tight distance.")
  
  expect_error(computeCanopy(NULL, tableName = 'batting', tightDistance = 0.1, looseDistance = 0.2, 
                             include=c('lgid','teamid','playerid'), id='id',
                             tableInfo = batting_info, test = TRUE),
               "Kmeans operates on one or more numeric variables.")
  
  expect_error(computeCanopy(NULL, tableName = 'batting', tightDistance = 0.1, looseDistance = 0.2,
                             include = c('ab','r','h'), id='id', idAlias = 'r',
                             tableInfo = batting_info, test = TRUE),
               "Id alias 'r' can't be one of variable names.")

})

canopyObj = structure(list(centers = structure(c(1.82130869409267, 1.9964865907332, 1.07179419347032, 
                                                 2.0199529975819, -0.369746427959872, 1.37959304595806), .Dim = c(2L, 3L), 
                                                 .Dimnames = list(c("1", "2"), 
                                                                  c("g", "h", "r"))), 
                             looseDistance = 2, 
                             tightDistance = 0.5, 
                             tableName = "batting", columns = c("g", "h", "r"), scale = TRUE, 
                             scaledTableName = "test_canopy_scaled", 
                             id = "playerid || '-' || stint || '-' || teamid || '-' || yearid", 
                             idAlias = "id", 
                             whereClause = " ", 
                             time = structure(c(0.0600000000000005,0.370000000000001, 53.0500000000002, NA, NA),
                                              class = "proc_time", .Names = c("user.self", "sys.self", "elapsed", "user.child", "sys.child"))),
                        .Names = c("centers", "looseDistance", "tightDistance", "tableName", "columns", "scale", 
                                   "scaledTableName", "id", "idAlias", "whereClause", "time"), 
                        class = "toacanopy")

test_that("computeCanopy SQL is correct", {
  
  expect_equal_normalized(computeCanopy(NULL, "batting", tightDistance = 0.5, looseDistance = 2,
                                        id="playerid || '-' || stint || '-' || teamid || '-' || yearid",
                                        include=c('g','r','h'), idAlias = 'id', scale = TRUE,
                                        scaledTableName = 'test_canopy_scaled',
                                        tableInfo = batting_info, test = TRUE),
"-- Data Prep: scale
DROP TABLE IF EXISTS test_canopy_scaled;
CREATE FACT TABLE test_canopy_scaled DISTRIBUTE BY HASH(id) AS 
       SELECT * FROM Scale(
       ON (SELECT playerid || '-' || stint || '-' || teamid || '-' || yearid id, g, h, r FROM batting ) AS input PARTITION BY ANY
       ON (SELECT * FROM ScaleMap (
       ON (SELECT playerid || '-' || stint || '-' || teamid || '-' || yearid id, g, h, r FROM batting )
    InputColumns ('g', 'h', 'r')
    -- MissValue ('OMIT')
    )) AS STATISTIC DIMENSION
       Method ('STD')
       Accumulate('id')
       GlobalScale ('false')
       InputColumns ('g', 'h', 'r')
     );
-- Run canopy;
SELECT * FROM Canopy(
       ON (SELECT 1) PARTITION BY 1
       InputTable('test_canopy_scaled')
       LooseDistance('2')
       TightDistance('0.5')
     );")
  
  expect_equal_normalized(computeCanopy(NULL, "batting", tightDistance = 0.5, looseDistance = 2,
                                        id="playerid || '-' || stint || '-' || teamid || '-' || yearid",
                                        include=c('g','r','h'), idAlias = 'id', scale = TRUE,
                                        scaledTableName = 'test_canopy_scaled',
                                        where = 'yearid >= 2000', tableInfo = batting_info, test = TRUE),
"-- Data Prep: scale
DROP TABLE IF EXISTS test_canopy_scaled;
CREATE FACT TABLE test_canopy_scaled DISTRIBUTE BY HASH(id) AS 
       SELECT * FROM Scale(
       ON (SELECT playerid || '-' || stint || '-' || teamid || '-' || yearid id, g, h, r FROM batting WHERE yearid >= 2000 ) AS input PARTITION BY ANY
       ON (SELECT * FROM ScaleMap (
       ON (SELECT playerid || '-' || stint || '-' || teamid || '-' || yearid id, g, h, r FROM batting WHERE yearid >= 2000 )
    InputColumns ('g', 'h', 'r')
    -- MissValue ('OMIT')
    )) AS STATISTIC DIMENSION
       Method ('STD')
       Accumulate('id')
       GlobalScale ('false')
       InputColumns ('g', 'h', 'r')
     );
-- Run canopy;
SELECT * FROM Canopy(
       ON (SELECT 1) PARTITION BY 1
       InputTable('test_canopy_scaled')
       LooseDistance('2')
       TightDistance('0.5')
     );")
  
  expect_equal_normalized(computeCanopy(NULL, "batting", tightDistance = 5, looseDistance = 50,
                                        id="playerid || '-' || stint || '-' || teamid || '-' || yearid",
                                        include=c('g','r','h'), idAlias = 'id', scale = FALSE,
                                        scaledTableName = 'test_canopy_scaled',
                                        tableInfo = batting_info, test = TRUE),
"-- Data Prep: omit nulls
DROP TABLE IF EXISTS test_canopy_scaled;
CREATE FACT TABLE test_canopy_scaled DISTRIBUTE BY HASH(id) AS 
       SELECT * FROM (SELECT playerid || '-' || stint || '-' || teamid || '-' || yearid id, g, h, r FROM batting ) d
      WHERE id IS NOT NULL AND g IS NOT NULL AND h IS NOT NULL AND r IS NOT NULL;
-- Run canopy;
SELECT * FROM Canopy(
       ON (SELECT 1) PARTITION BY 1
       InputTable('test_canopy_scaled')
       LooseDistance('50')
       TightDistance('5')
     );")
  
  expect_equal_normalized(computeCanopy(NULL, "batting", tightDistance = 5, looseDistance = 50,
                                        id="playerid || '-' || stint || '-' || teamid || '-' || yearid",
                                        include=c('g','r','h'), idAlias = 'id', scale = FALSE,
                                        scaledTableName = 'test_canopy_scaled',
                                        where = 'yearid >= 2000', tableInfo = batting_info, test = TRUE),
"-- Data Prep: omit nulls
DROP TABLE IF EXISTS test_canopy_scaled;
CREATE FACT TABLE test_canopy_scaled DISTRIBUTE BY HASH(id) AS 
       SELECT * FROM (SELECT playerid || '-' || stint || '-' || teamid || '-' || yearid id, g, h, r FROM batting WHERE yearid >= 2000 ) d
      WHERE id IS NOT NULL AND g IS NOT NULL AND h IS NOT NULL AND r IS NOT NULL;
-- Run canopy;
SELECT * FROM Canopy(
       ON (SELECT 1) PARTITION BY 1
       InputTable('test_canopy_scaled')
       LooseDistance('50')
       TightDistance('5')
     );")
  
  expect_equal_normalized(computeCanopy(NULL, canopy = canopyObj, looseDistance = 2, tightDistance = 0.5, 
                                        tableInfo = batting_info, test = TRUE),
";
-- Run canopy;
SELECT * FROM Canopy(
       ON (SELECT 1) PARTITION BY 1
       InputTable('test_canopy_scaled')
       LooseDistance('2')
       TightDistance('0.5')
     );")
})

  
test_that("computeKmeans SQL with canopy is correct", {
  
  expect_equal_normalized(computeKmeans(NULL, centers = canopyObj,
                                        tempTableName = 'test_canopy_temp',
                                        centroidTableName = 'test_canopy_centroids',
                                        tableInfo = batting_info, test = TRUE),
";
--;
-- Run k-means;
DROP TABLE IF EXISTS test_canopy_centroids;
SELECT * FROM kmeans(
      ON (SELECT 1)
      PARTITION BY 1
      INPUTTABLE('test_canopy_scaled')
      OUTPUTTABLE('test_canopy_centroids')
      MEANS('1.82130869409267_1.07179419347032_-0.369746427959872', '1.9964865907332_2.0199529975819_1.37959304595806')
      THRESHOLD('0.0395')
      MAXITERNUM('10')
    );
--;
-- Run cluster assignment, cluster stats, and within-cluster sum of squares;
SELECT c1.*, c2.withinss  
       FROM (SELECT clusterid, means, COUNT(*) cnt          FROM (SELECT c.clusterid, c.\"g h r\" means, d.* 
      FROM test_canopy_centroids c JOIN 
    kmeansplot (
      ON test_canopy_scaled PARTITION BY ANY
      ON test_canopy_centroids DIMENSION
      centroidsTable('test_canopy_centroids')
    ) kmp ON (c.clusterid = kmp.clusterid) JOIN 
    (SELECT playerid || '-' || stint || '-' || teamid || '-' || yearid id, * FROM batting ) d on (kmp.id = d.id)
                    ) clustered_data
              GROUP BY clusterid, means
            ) c1 JOIN ( 
            SELECT 0 clusterid, SUM(distance::double ^ 2) withinss FROM VectorDistance(
       ON (
         SELECT clusterid, id, variable, coalesce(value_double, value_long, value_str::double) value
           FROM unpivot(
                  ON (SELECT d.* 
                        FROM kmeansplot (
                               ON test_canopy_scaled PARTITION BY ANY
                               ON test_canopy_centroids DIMENSION
                               centroidsTable('test_canopy_centroids')
                             ) d 
                       WHERE clusterid = 0
                  )
                  COLSTOUNPIVOT('g', 'h', 'r')
                  COLSTOACCUMULATE('id','clusterid')
                  ATTRIBUTECOLUMNNAME('variable')
                  VALUECOLUMNNAME('value')
                  KEEPINPUTCOLUMNTYPES('true')
                )
       ) AS target PARTITION BY id
       ON (
         SELECT *, regexp_split_to_table(\"g h r\", ' ')::numeric value, regexp_split_to_table('g, h, r', ', ') variable 
           FROM test_canopy_centroids WHERE clusterid = 0
       ) AS ref DIMENSION
       TARGETIDCOLUMNS('id')
       TARGETFEATURECOLUMN('variable')
       TARGETVALUECOLUMN('value')
       REFIDCOLUMNS('clusterid')
       REFFEATURECOLUMN('variable')
       REFVALUECOLUMN('value')
       MEASURE('Euclidean')
     )
UNION ALL
SELECT 1 clusterid, SUM(distance::double ^ 2) withinss FROM VectorDistance(
       ON (
         SELECT clusterid, id, variable, coalesce(value_double, value_long, value_str::double) value
           FROM unpivot(
                  ON (SELECT d.* 
                        FROM kmeansplot (
                               ON test_canopy_scaled PARTITION BY ANY
                               ON test_canopy_centroids DIMENSION
                               centroidsTable('test_canopy_centroids')
                             ) d 
                       WHERE clusterid = 1
                  )
                  COLSTOUNPIVOT('g', 'h', 'r')
                  COLSTOACCUMULATE('id','clusterid')
                  ATTRIBUTECOLUMNNAME('variable')
                  VALUECOLUMNNAME('value')
                  KEEPINPUTCOLUMNTYPES('true')
                )
       ) AS target PARTITION BY id
       ON (
         SELECT *, regexp_split_to_table(\"g h r\", ' ')::numeric value, regexp_split_to_table('g, h, r', ', ') variable 
           FROM test_canopy_centroids WHERE clusterid = 1
       ) AS ref DIMENSION
       TARGETIDCOLUMNS('id')
       TARGETFEATURECOLUMN('variable')
       TARGETVALUECOLUMN('value')
       REFIDCOLUMNS('clusterid')
       REFFEATURECOLUMN('variable')
       REFVALUECOLUMN('value')
       MEASURE('Euclidean')
     )
            ) c2 ON (c1.clusterid = c2.clusterid)
      ORDER BY clusterid;
--;
-- Compute Total Sum of Squares;
SELECT SUM(distance::double ^ 2) totss FROM VectorDistance(
       ON (SELECT id, variable, coalesce(value_double, value_long, value_str::double) value
             FROM unpivot(
               ON test_canopy_scaled
               COLSTOUNPIVOT('g', 'h', 'r')
               COLSTOACCUMULATE('id')
               ATTRIBUTECOLUMNNAME('variable')
               VALUECOLUMNNAME('value')
               KEEPINPUTCOLUMNTYPES('true')
             ) 
       ) AS target PARTITION BY id
       ON (SELECT id, variable, value_double
             FROM unpivot(
               ON (SELECT 1 id, 0.0::double g, 0.0::double h, 0.0::double r)
               COLSTOUNPIVOT('g', 'h', 'r')
               COLSTOACCUMULATE('id')
               ATTRIBUTECOLUMNNAME('variable')
               VALUECOLUMNNAME('value')
               KEEPINPUTCOLUMNTYPES('true')
             )
       ) AS ref DIMENSION
       TARGETIDCOLUMNS('id')
       TARGETFEATURECOLUMN('variable')
       TARGETVALUECOLUMN('value')
       REFIDCOLUMNS('id')
       REFFEATURECOLUMN('variable')
       REFVALUECOLUMN('value_double')
       MEASURE('Euclidean')
     );")
  
  expect_equal_normalized(computeKmeans(NULL, centers = canopyObj, persist = TRUE,
                                        tempTableName = 'test_canopy_temp',
                                        centroidTableName = 'test_canopy_centroids',
                                        clusteredTableName = 'test_canopy_clustered',
                                        tableInfo = batting_info, test = TRUE),
";
--;
-- Run k-means;
DROP TABLE IF EXISTS test_canopy_centroids;
DROP TABLE IF EXISTS test_canopy_temp;
SELECT * FROM kmeans(
      ON (SELECT 1)
      PARTITION BY 1
      INPUTTABLE('test_canopy_scaled')
      OUTPUTTABLE('test_canopy_centroids')
      ClusteredOutput('test_canopy_temp')
      MEANS('1.82130869409267_1.07179419347032_-0.369746427959872', '1.9964865907332_2.0199529975819_1.37959304595806')
      THRESHOLD('0.0395')
      MAXITERNUM('10')
    );
--;
-- Combine clustered ids with data;
DROP TABLE IF EXISTS test_canopy_clustered;
CREATE FACT TABLE test_canopy_clustered DISTRIBUTE BY HASH(id) AS 
         SELECT d.*, c.clusterid 
           FROM test_canopy_temp c JOIN 
                test_canopy_scaled d ON (c.id = d.id);
DROP TABLE IF EXISTS test_canopy_temp;
--;
-- Run cluster assignment, cluster stats, and within-cluster sum of squares;
SELECT c1.*, c2.withinss  
       FROM (SELECT clusterid, means, COUNT(*) cnt          FROM (SELECT c.clusterid, c.\"g h r\" means, d.* 
      FROM test_canopy_centroids c JOIN 
    test_canopy_clustered kmp ON (c.clusterid = kmp.clusterid) JOIN 
    (SELECT playerid || '-' || stint || '-' || teamid || '-' || yearid id, * FROM batting ) d on (kmp.id = d.id)
                    ) clustered_data
              GROUP BY clusterid, means
            ) c1 JOIN ( 
            SELECT 0 clusterid, SUM(distance::double ^ 2) withinss FROM VectorDistance(
       ON (
         SELECT clusterid, id, variable, coalesce(value_double, value_long, value_str::double) value
           FROM unpivot(
                  ON (SELECT d.* 
                        FROM test_canopy_clustered d 
                       WHERE clusterid = 0
                  )
                  COLSTOUNPIVOT('g', 'h', 'r')
                  COLSTOACCUMULATE('id','clusterid')
                  ATTRIBUTECOLUMNNAME('variable')
                  VALUECOLUMNNAME('value')
                  KEEPINPUTCOLUMNTYPES('true')
                )
       ) AS target PARTITION BY id
       ON (
         SELECT *, regexp_split_to_table(\"g h r\", ' ')::numeric value, regexp_split_to_table('g, h, r', ', ') variable 
           FROM test_canopy_centroids WHERE clusterid = 0
       ) AS ref DIMENSION
       TARGETIDCOLUMNS('id')
       TARGETFEATURECOLUMN('variable')
       TARGETVALUECOLUMN('value')
       REFIDCOLUMNS('clusterid')
       REFFEATURECOLUMN('variable')
       REFVALUECOLUMN('value')
       MEASURE('Euclidean')
     )
UNION ALL
SELECT 1 clusterid, SUM(distance::double ^ 2) withinss FROM VectorDistance(
       ON (
         SELECT clusterid, id, variable, coalesce(value_double, value_long, value_str::double) value
           FROM unpivot(
                  ON (SELECT d.* 
                        FROM test_canopy_clustered d 
                       WHERE clusterid = 1
                  )
                  COLSTOUNPIVOT('g', 'h', 'r')
                  COLSTOACCUMULATE('id','clusterid')
                  ATTRIBUTECOLUMNNAME('variable')
                  VALUECOLUMNNAME('value')
                  KEEPINPUTCOLUMNTYPES('true')
                )
       ) AS target PARTITION BY id
       ON (
         SELECT *, regexp_split_to_table(\"g h r\", ' ')::numeric value, regexp_split_to_table('g, h, r', ', ') variable 
           FROM test_canopy_centroids WHERE clusterid = 1
       ) AS ref DIMENSION
       TARGETIDCOLUMNS('id')
       TARGETFEATURECOLUMN('variable')
       TARGETVALUECOLUMN('value')
       REFIDCOLUMNS('clusterid')
       REFFEATURECOLUMN('variable')
       REFVALUECOLUMN('value')
       MEASURE('Euclidean')
     )
            ) c2 ON (c1.clusterid = c2.clusterid)
      ORDER BY clusterid;
--;
-- Compute Total Sum of Squares;
SELECT SUM(distance::double ^ 2) totss FROM VectorDistance(
       ON (SELECT id, variable, coalesce(value_double, value_long, value_str::double) value
             FROM unpivot(
               ON test_canopy_scaled
               COLSTOUNPIVOT('g', 'h', 'r')
               COLSTOACCUMULATE('id')
               ATTRIBUTECOLUMNNAME('variable')
               VALUECOLUMNNAME('value')
               KEEPINPUTCOLUMNTYPES('true')
             ) 
       ) AS target PARTITION BY id
       ON (SELECT id, variable, value_double
             FROM unpivot(
               ON (SELECT 1 id, 0.0::double g, 0.0::double h, 0.0::double r)
               COLSTOUNPIVOT('g', 'h', 'r')
               COLSTOACCUMULATE('id')
               ATTRIBUTECOLUMNNAME('variable')
               VALUECOLUMNNAME('value')
               KEEPINPUTCOLUMNTYPES('true')
             )
       ) AS ref DIMENSION
       TARGETIDCOLUMNS('id')
       TARGETFEATURECOLUMN('variable')
       TARGETVALUECOLUMN('value')
       REFIDCOLUMNS('id')
       REFFEATURECOLUMN('variable')
       REFVALUECOLUMN('value_double')
       MEASURE('Euclidean')
     );")
  
})