context("computeKmeans") 

batting_info = dget("_battingInfo.dat")

test_that("computeKmeans throws errors", {
  
  expect_error(computeKmeans(NULL, tableName="fielding", test=TRUE),
               "Must provide tableInfo when test==TRUE")
  
  expect_error(computeKmeans(NULL, tableName="batting", tableInfo=batting_info, id="id", include=c('lgid','playerid')),
               "Kmeans operates on one or more numeric variables.")
  
  expect_error(computeKmeans(NULL, tableName="batting", tableInfo=batting_info, id="id", idAlias="g"),
               "Id alias 'g' can't be one of variable names")
  
  expect_error(computeKmeans(NULL, tableName="batting", tableInfo=batting_info, id="id", include=c('g','h','r'),
                             idAlias="g"), "Id alias 'g' can't be one of variable names")
  
})


test_that("computeClusterSample throws errors", {
  
  expect_error(computeClusterSample(NULL),
               "Kmeans object must be specified.")
  
  expect_error(computeClusterSample(NULL, NULL),
               "Sample fraction or sample size must be specified.")
})


test_that("computeKmeans SQL is correct", {
  
  expect_equal_normalized(computeKmeans(NULL, "batting", batting_info, 
                                        "playerid || '-' || stint || '-' || teamid || '-' || yearid", c('g','ab','r','h'),
                                        aggregates = c("COUNT(*) cnt", "AVG(g) avg_g", "AVG(ab) ab", "AVG(r) avg_r", "AVG(h) avg_h"),
                                        scaledTableName='kmeans_test_scaled', centroidTableName='kmeans_test_centroids', 
                                        schema='baseball', test=TRUE),
                          "DROP TABLE IF EXISTS baseball.kmeans_test_scaled;
                           CREATE FACT TABLE baseball.kmeans_test_scaled DISTRIBUTE BY HASH(id) AS 
                           SELECT * FROM Scale(
                             ON (SELECT playerid || '-' || stint || '-' || teamid || '-' || yearid id, g, ab, r, h FROM batting ) AS input PARTITION BY ANY
                             ON (SELECT * FROM ScaleMap (
                                   ON (SELECT playerid || '-' || stint || '-' || teamid || '-' || yearid id, g, ab, r, h FROM batting )
                                   InputColumns ('g', 'ab', 'r', 'h')
                                   -- MissValue ('OMIT')
                                )) AS STATISTIC DIMENSION
                             Method ('STD')
                             Accumulate('id')
                             GlobalScale ('false')
                             InputColumns ('g', 'ab', 'r', 'h')
                           );
                           DROP TABLE IF EXISTS baseball.kmeans_test_centroids;
                           SELECT * FROM kmeans(
                             ON (SELECT 1)
                             PARTITION BY 1
                             INPUTTABLE('baseball.kmeans_test_scaled')
                             OUTPUTTABLE('baseball.kmeans_test_centroids')
                             NUMBERK('6')
                             THRESHOLD('0.001')
                             MAXITERNUM('100')
                          );
                          SELECT clusterid, means, COUNT(*) cnt, AVG(g) avg_g, AVG(ab) ab, AVG(r) avg_r, AVG(h) avg_h  
                            FROM (SELECT c.clusterid, c.means, d.* 
                                    FROM baseball.kmeans_test_centroids c JOIN 
                                         kmeansplot (
                                           ON baseball.kmeans_test_scaled PARTITION BY ANY
                                           ON baseball.kmeans_test_centroids DIMENSION
                                           centroidsTable('baseball.kmeans_test_centroids')
                                         ) kmp ON (c.clusterid = kmp.clusterid) JOIN 
                                         (SELECT playerid || '-' || stint || '-' || teamid || '-' || yearid id, g, ab, r, h FROM batting ) d on (kmp.id = d.id)
                          ) clustered_data
                          GROUP BY clusterid, means;")
})


test_that("computeClusterSample SQL is correct", {
  
  kmeans_obj <- structure(list(centers = 6,
                               tableName = "batting",
                               columns = c('g', 'r', 'h'),
                               scaledTableName = "baseball.kmeans_test_scaled",
                               centroidTableName = "baseball.kmeans_test_centroids",
                               id = "playerid || '-' || stint || '-' || teamid || '-' || yearid",
                               idAlias = "id",
                               whereClause=" WHERE yearid > 2010  "))
  
  expect_equal_normalized(computeClusterSample(NULL, kmeans_obj, 0.01, test=TRUE), 
                          "SELECT * FROM antiselect(
         ON (SELECT * FROM sample(
             ON (SELECT clusterid, d.* FROM kmeansplot(
                   ON baseball.kmeans_test_scaled PARTITION BY ANY
                   ON baseball.kmeans_test_centroids DIMENSION
                   centroidsTable('baseball.kmeans_test_centroids')
                 ) kmp JOIN (SELECT playerid || '-' || stint || '-' || teamid || '-' || yearid id, g, r, h 
                               FROM batting 
                              WHERE yearid > 2010  ) d 
                       ON (kmp.id = d.id)
                 WHERE clusterid != -1
             )
             CONDITIONONCOLUMN('clusterid')
             CONDITIONON('0','1','2','3','4','5')
             SAMPLEFRACTION('0.01')
       )
         )
         EXCLUDE('id')
       )",
                          info="Sampling fraction unscaled data without row id.")
  
  expect_equal_normalized(computeClusterSample(NULL, kmeans_obj, 0.01, includeId=TRUE, test=TRUE), 
                          "SELECT * FROM sample(
             ON (SELECT clusterid, d.* FROM kmeansplot(
                   ON baseball.kmeans_test_scaled PARTITION BY ANY
                   ON baseball.kmeans_test_centroids DIMENSION
                   centroidsTable('baseball.kmeans_test_centroids')
                 ) kmp JOIN (SELECT playerid || '-' || stint || '-' || teamid || '-' || yearid id, g, r, h 
                               FROM batting 
                              WHERE yearid > 2010  ) d 
                       ON (kmp.id = d.id)
                 WHERE clusterid != -1
             )
             CONDITIONONCOLUMN('clusterid')
             CONDITIONON('0','1','2','3','4','5')
             SAMPLEFRACTION('0.01')
       )",
                          info="Sampling fraction unscaled data with row id.")
  
  expect_equal_normalized(computeClusterSample(NULL, kmeans_obj, 0.01, scaled=TRUE, test=TRUE), 
      "SELECT * FROM antiselect(
         ON (SELECT * FROM sample(
             ON (SELECT d.* FROM kmeansplot(
                   ON baseball.kmeans_test_scaled PARTITION BY ANY
                   ON baseball.kmeans_test_centroids DIMENSION
                   centroidsTable('baseball.kmeans_test_centroids')
                 ) d
                 WHERE clusterid != -1
             )
             CONDITIONONCOLUMN('clusterid')
             CONDITIONON('0','1','2','3','4','5')
             SAMPLEFRACTION('0.01')
       )
         )
         EXCLUDE('id')
       )",
      info="Sampling fraction scaled data without row id.")
  
  expect_equal_normalized(computeClusterSample(NULL, kmeans_obj, 0.01, scaled=TRUE, includeId=TRUE, test=TRUE), 
          "SELECT * FROM sample(
             ON (SELECT d.* FROM kmeansplot(
                   ON baseball.kmeans_test_scaled PARTITION BY ANY
                   ON baseball.kmeans_test_centroids DIMENSION
                   centroidsTable('baseball.kmeans_test_centroids')
                 ) d
                 WHERE clusterid != -1
             )
             CONDITIONONCOLUMN('clusterid')
             CONDITIONON('0','1','2','3','4','5')
             SAMPLEFRACTION('0.01')
       )",
          info="Sampling fraction scaled with row id.")
  
  expect_equal_normalized(computeClusterSample(NULL, kmeans_obj, sampleSize=1000, test=TRUE),
"SELECT * FROM antiselect(
          ON 
            (WITH stratum_counts AS (
          SELECT clusterid stratum, count(*) stratum_count 
            FROM kmeansplot(
              ON baseball.kmeans_test_scaled PARTITION BY ANY
              ON baseball.kmeans_test_centroids DIMENSION
              centroidsTable('baseball.kmeans_test_centroids')
            ) 
           WHERE clusterid != -1
           GROUP BY 1
         )
         SELECT * FROM sample (
           ON (SELECT clusterid, d.* FROM kmeansplot(
             ON baseball.kmeans_test_scaled PARTITION BY ANY
             ON baseball.kmeans_test_centroids DIMENSION
             centroidsTable('baseball.kmeans_test_centroids')
                ) kmp JOIN (SELECT playerid || '-' || stint || '-' || teamid || '-' || yearid id, g, r, h 
                                 FROM batting 
                                WHERE yearid > 2010  ) d 
                         ON (kmp.id = d.id)
                   WHERE clusterid != -1
               ) AS data PARTITION BY ANY
               ON stratum_counts AS summary DIMENSION
               CONDITIONONCOLUMN('clusterid')
               CONDITIONON('0','1','2','3','4','5')
               ApproximateSampleSize('1000')
             )
             )
             EXCLUDE('id')
          )",
          info="Sampling size unscaled data without row id.")
  
  expect_equal_normalized(computeClusterSample(NULL, kmeans_obj, sampleSize = 1000, includeId=TRUE, test=TRUE),
          "WITH stratum_counts AS (
            SELECT clusterid stratum, count(*) stratum_count 
              FROM kmeansplot(
                ON baseball.kmeans_test_scaled PARTITION BY ANY
                ON baseball.kmeans_test_centroids DIMENSION
                centroidsTable('baseball.kmeans_test_centroids')
              ) 
             WHERE clusterid != -1
            GROUP BY 1
           )
           SELECT * FROM sample (
             ON (SELECT clusterid, d.* FROM kmeansplot(
               ON baseball.kmeans_test_scaled PARTITION BY ANY
               ON baseball.kmeans_test_centroids DIMENSION
               centroidsTable('baseball.kmeans_test_centroids')
                 ) kmp JOIN (SELECT playerid || '-' || stint || '-' || teamid || '-' || yearid id, g, r, h 
                                 FROM batting 
                                WHERE yearid > 2010  ) d 
                         ON (kmp.id = d.id) 
                 WHERE clusterid != -1
             ) AS data PARTITION BY ANY
             ON stratum_counts AS summary DIMENSION
             CONDITIONONCOLUMN('clusterid')
             CONDITIONON('0','1','2','3','4','5')
             ApproximateSampleSize('1000')
           )",
          info="Sampling size unscaled data with id.")
  
  expect_equal_normalized(computeClusterSample(NULL, kmeans_obj, sampleSize=1000, scaled=TRUE, test=TRUE),
          "SELECT * FROM antiselect(
             ON 
             (WITH stratum_counts AS (
               SELECT clusterid stratum, count(*) stratum_count 
                 FROM kmeansplot(
                   ON baseball.kmeans_test_scaled PARTITION BY ANY
                   ON baseball.kmeans_test_centroids DIMENSION
                   centroidsTable('baseball.kmeans_test_centroids')
                 ) 
                WHERE clusterid != -1
               GROUP BY 1
             )
             SELECT * FROM sample (
               ON (SELECT d.* FROM kmeansplot(
                 ON baseball.kmeans_test_scaled PARTITION BY ANY
                 ON baseball.kmeans_test_centroids DIMENSION
                 centroidsTable('baseball.kmeans_test_centroids')
                  ) d
                  WHERE clusterid != -1
               ) AS data PARTITION BY ANY
               ON stratum_counts AS summary DIMENSION
               CONDITIONONCOLUMN('clusterid')
               CONDITIONON('0','1','2','3','4','5')
               ApproximateSampleSize('1000')
             )
             )
             EXCLUDE('id')
           )",
          info="Sampling size scaled data without id.")
  
  expect_equal_normalized(computeClusterSample(NULL, kmeans_obj, sampleSize = 1000, scaled=TRUE, includeId=TRUE, test=TRUE),
          "WITH stratum_counts AS (
            SELECT clusterid stratum, count(*) stratum_count 
              FROM kmeansplot(
                ON baseball.kmeans_test_scaled PARTITION BY ANY
                ON baseball.kmeans_test_centroids DIMENSION
                centroidsTable('baseball.kmeans_test_centroids')
              ) 
             WHERE clusterid != -1
            GROUP BY 1
           )
           SELECT * FROM sample (
             ON (SELECT d.* FROM kmeansplot(
               ON baseball.kmeans_test_scaled PARTITION BY ANY
               ON baseball.kmeans_test_centroids DIMENSION
               centroidsTable('baseball.kmeans_test_centroids')
                 ) d
                 WHERE clusterid != -1
             ) AS data PARTITION BY ANY
             ON stratum_counts AS summary DIMENSION
             CONDITIONONCOLUMN('clusterid')
             CONDITIONON('0','1','2','3','4','5')
             ApproximateSampleSize('1000')
           )",
          info="Sampling size scaled data with id.")
})