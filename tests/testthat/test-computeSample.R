context("computeSample")

test_that("computeSample throws errors", {
  
  expect_error(computeSample(channel=NULL),
               "Table name must be specified.")
  
  expect_error(computeSample(channel=NULL, tableName="faketable"),
               "Sample fraction or sample size must be specified.")
  
  expect_error(computeSample(channel=NULL, tableName="faketable", sampleFraction=0.1,
                             except = c("some", "columns", "defined", "without", "include"),
                             test=FALSE),
               "Can't test without include but with except.")
  
  expect_error(computeSample(channel=NULL, tableName="faketable", sampleFraction=1.000001,
                             test=FALSE),
               "sampleFraction <= 1 is not TRUE")
  
  expect_error(computeSample(channel=NULL, tableName="faketable", sampleFraction=c(1.000001,0.01),
                             test=FALSE),
               "sampleFraction <= 1 are not all TRUE")
  
  expect_error(computeSample(channel=NULL, tableName="faketable", sampleFraction=-0.000001,
                             test=FALSE),
               "sampleFraction >= 0 is not TRUE")
  
  expect_error(computeSample(channel=NULL, tableName="faketable", sampleFraction=c(1, 0, -0.000001),
                             test=FALSE),
               "sampleFraction >= 0 are not all TRUE")
  
  expect_error(computeSample(channel=NULL, tableName='faketable', sampleSize = 1000,
                             conditionColumn = 'conditioncol', conditionStratum='CASE WHEN THEN ELSE END'),
               "Both condition column and strata can't be defined. Use either one or another.")
  
  expect_error(computeSample(channel=NULL, tableName="faketable", sampleFraction=0.1,
                             conditionColumn ='fake column', test=FALSE),
               "Both condition column and condition values must be either present or NULLs.")
  
  expect_error(computeSample(channel=NULL, tableName="faketable", sampleFraction=0.1,
                             conditionValues =c('fake', 'values'), test=FALSE),
               "Both condition column and condition values must be either present or NULLs.")
  
  expect_error(computeSample(channel=NULL, tableName="faketable", sampleFraction=c(0.1,0.2,0.4),
                             conditionColumn = 'condition', conditionValues =c('fake', 'values'), 
                             test=FALSE),
               "Number of fractions must match the number of condition values.")
  
  expect_error(computeSample(channel=NULL, tableName="faketable", sampleSize=c(1,2,4),
                             conditionColumn = 'condition', conditionValues =c('fake', 'values'), 
                             test=FALSE),
               "Number of sample sizes must match the number of condition values.")
  
  expect_error(computeSample(channel=NULL, tableName="faketable", sampleFraction=0.000001),
               "Connection is not valid RODBC object.")
  
})

test_that("computeSample SQL is correct", {
  
  expect_equal_normalized(computeSample(channel=NULL, tableName='public.batting_enh', 
                                        sampleFraction=0.01, test=TRUE),
                          "SELECT * FROM sample(
                                           ON (SELECT  *  FROM public.batting_enh )  
                                           SampleFraction('0.01'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName='public.batting_enh', 
                                        sampleFraction=0.01, sampleSize=100, test=TRUE),
                          "SELECT * FROM sample(
                                           ON (SELECT  *  FROM public.batting_enh )  
                                           SampleFraction('0.01'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName='public.batting_enh', 
                                        sampleFraction=0.01, where="lgid='AL'", test=TRUE),
                          "SELECT * FROM sample(
                                           ON (SELECT  *  FROM public.batting_enh WHERE lgid='AL' )  
                                           SampleFraction('0.01'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName='public.batting_enh', 
                                        sampleFraction=0.01, include=c('decadeid','g','ab','r','h'),
                                        where="lgid='AL'", test=TRUE),
                          "SELECT * FROM sample(
                                           ON (SELECT decadeid, g, ab, r, h FROM public.batting_enh WHERE lgid='AL' )  
                                           SampleFraction('0.01'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName='public.batting_enh',
                                        sampleSize=1000, test=TRUE),
                          "SELECT * 
                             FROM sample(
                               ON (SELECT  *  FROM public.batting_enh  ) 
                                 AS DATA PARTITION BY ANY                           
                               ON (SELECT COUNT(*) as stratum_count FROM public.batting_enh )  
                                 AS SUMMARY DIMENSION
                               ApproximateSampleSize('1000'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName='public.batting_enh',
                                        sampleSize=1000, where="lgid='NL'", test=TRUE),
                          "SELECT * 
                             FROM sample(
                               ON (SELECT  *  FROM public.batting_enh WHERE lgid='NL' )
                                 AS DATA PARTITION BY ANY                           
                               ON (SELECT COUNT(*) as stratum_count FROM public.batting_enh WHERE lgid='NL' )  
                                 AS SUMMARY DIMENSION
                               ApproximateSampleSize('1000'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName='public.batting_enh',
                                        sampleSize=1000, include=c('decadeid','g','ab','r','h'),
                                        where="lgid='NL'", test=TRUE),
                          "SELECT * 
                             FROM sample(
                               ON (SELECT  decadeid, g, ab, r, h  FROM public.batting_enh WHERE lgid='NL' )
                                 AS DATA PARTITION BY ANY                           
                               ON (SELECT COUNT(*) as stratum_count FROM public.batting_enh WHERE lgid='NL' )  
                                 AS SUMMARY DIMENSION
                               ApproximateSampleSize('1000'))")
})

test_that("computeSample SQL with condition is correct", {
  
  expect_equal_normalized(computeSample(channel=NULL, tableName = 'public.batting_enh',
                                        sampleFraction = 0.01, 
                                        conditionColumn = 'decadeid', conditionValues = c(1990,2000,2010),
                                        test = TRUE), 
                          "SELECT *   
                             FROM sample(
                               ON (SELECT  *  FROM public.batting_enh  )  
                               SampleFraction('0.01') 
                               ConditionOnColumn('decadeid')
                               ConditionOn('1990', '2000', '2010'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName = 'public.batting_enh',
                                        sampleFraction = c(0.01,0.01,0.02), 
                                        conditionColumn = 'decadeid', conditionValues = c(1990,2000,2010),
                                        test = TRUE), 
                          "SELECT *   
                             FROM sample(
                               ON (SELECT  *  FROM public.batting_enh  )  
                               SampleFraction('0.01', '0.01', '0.02') 
                               ConditionOnColumn('decadeid')
                               ConditionOn('1990', '2000', '2010'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName = 'public.batting_enh',
                                        sampleFraction = 0.01, 
                                        conditionColumn = 'decadeid', conditionValues = c(1990,2000,2010),
                                        where="lgid='NL'", test = TRUE), 
                          "SELECT *   
                             FROM sample(
                               ON (SELECT  *  FROM public.batting_enh WHERE lgid='NL' )  
                               SampleFraction('0.01') 
                               ConditionOnColumn('decadeid')
                               ConditionOn('1990', '2000', '2010'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName = 'public.batting_enh',
                                        sampleFraction = c(0.01,0.01,0.02), 
                                        conditionColumn = 'decadeid', conditionValues = c(1990,2000,2010),
                                        where="lgid='NL'", test = TRUE), 
                          "SELECT *   
                             FROM sample(
                               ON (SELECT  *  FROM public.batting_enh WHERE lgid='NL' )  
                               SampleFraction('0.01', '0.01', '0.02') 
                               ConditionOnColumn('decadeid')
                               ConditionOn('1990', '2000', '2010'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName = 'public.batting_enh',
                                        sampleFraction = c(0.01,0.01,0.02), 
                                        include=c('decadeid','g','ab','r','h'),
                                        conditionColumn = 'decadeid', conditionValues = c(1990,2000,2010),
                                        where="lgid='NL'", test = TRUE), 
                          "SELECT *   
                             FROM sample(
                               ON (SELECT  decadeid, g, ab, r, h FROM public.batting_enh WHERE lgid='NL' )  
                               SampleFraction('0.01', '0.01', '0.02') 
                               ConditionOnColumn('decadeid')
                               ConditionOn('1990', '2000', '2010'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName = 'public.batting_enh',
                                        sampleSize = 100, 
                                        conditionColumn = 'decadeid', conditionValues = c(1990,2000,2010),
                                        test = TRUE),
                          "SELECT * 
                             FROM sample(
                               ON (SELECT  *  FROM public.batting_enh  ) 
                                 AS DATA PARTITION BY ANY
                               ON (SELECT decadeid as stratum, COUNT(*) as stratum_count FROM public.batting_enh GROUP BY 1) 
                                 AS SUMMARY DIMENSION
                               ApproximateSampleSize('100') 
                               ConditionOnColumn('decadeid')
                               ConditionOn('1990', '2000', '2010'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName = 'public.batting_enh',
                                        sampleSize = c(20,20,30), 
                                        conditionColumn = 'decadeid', conditionValues = c(1990,2000,2010),
                                        test = TRUE),
                          "SELECT * 
                             FROM sample(
                               ON (SELECT  *  FROM public.batting_enh  ) 
                                 AS DATA PARTITION BY ANY
                               ON (SELECT decadeid as stratum, COUNT(*) as stratum_count FROM public.batting_enh GROUP BY 1) 
                                 AS SUMMARY DIMENSION
                               ApproximateSampleSize('20', '20', '30') 
                               ConditionOnColumn('decadeid')
                               ConditionOn('1990', '2000', '2010'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName = 'public.batting_enh',
                                        sampleSize = 100, 
                                        conditionColumn = 'decadeid', conditionValues = c(1990,2000,2010),
                                        where="lgid='NL'", test = TRUE),
                          "SELECT * FROM sample(
                           ON (SELECT  *  FROM public.batting_enh WHERE lgid='NL' ) 
                             AS DATA PARTITION BY ANY
                           ON (SELECT decadeid as stratum, COUNT(*) as stratum_count FROM public.batting_enh WHERE lgid='NL' GROUP BY 1) 
                             AS SUMMARY DIMENSION
                           ApproximateSampleSize('100') 
                           ConditionOnColumn('decadeid')
                           ConditionOn('1990', '2000', '2010'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName = 'public.batting_enh',
                                        sampleSize = c(20,20,30), 
                                        conditionColumn = 'decadeid', conditionValues = c(1990,2000,2010),
                                        where="lgid='NL'", test = TRUE),
                          "SELECT * 
                             FROM sample(
                               ON (SELECT  *  FROM public.batting_enh WHERE lgid='NL' ) 
                                 AS DATA PARTITION BY ANY
                               ON (SELECT decadeid as stratum, COUNT(*) as stratum_count FROM public.batting_enh WHERE lgid='NL' GROUP BY 1) 
                                 AS SUMMARY DIMENSION
                               ApproximateSampleSize('20', '20', '30') 
                               ConditionOnColumn('decadeid')
                               ConditionOn('1990', '2000', '2010'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName = 'public.batting_enh',
                                        sampleSize = c(20,20,30), 
                                        include=c('decadeid','g','ab','r','h'),
                                        conditionColumn = 'decadeid', conditionValues = c(1990,2000,2010),
                                        where="lgid='NL'", test = TRUE),
                          "SELECT * 
                             FROM sample(
                               ON (SELECT decadeid, g, ab, r, h FROM public.batting_enh WHERE lgid='NL' ) 
                                 AS DATA PARTITION BY ANY
                               ON (SELECT decadeid as stratum, COUNT(*) as stratum_count FROM public.batting_enh WHERE lgid='NL' GROUP BY 1) 
                                 AS SUMMARY DIMENSION
                               ApproximateSampleSize('20', '20', '30') 
                               ConditionOnColumn('decadeid')
                               ConditionOn('1990', '2000', '2010'))")
})

test_that("computeSample SQL with stratum is correct", {
  
  expect_equal_normalized(computeSample(channel=NULL, tableName = 'public.batting_enh',
                                        sampleFraction = 0.01, 
                                        conditionStratum = "yearid % 2", 
                                        conditionValues = c('0','1'),
                                        test = TRUE),
                          "SELECT * 
                             FROM sample(
                               ON (SELECT  * , yearid % 2 as stratum  FROM public.batting_enh ) 
                               SampleFraction('0.01') 
                               ConditionOnColumn('stratum')
                               ConditionOn('0', '1'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName = 'public.batting_enh',
                                        sampleFraction = c(0.01,0.02), 
                                        conditionStratum = "yearid % 2", 
                                        conditionValues = c('0','1'),
                                        test = TRUE),
                          "SELECT * 
                             FROM sample(
                               ON (SELECT  * , yearid % 2 as stratum  FROM public.batting_enh ) 
                               SampleFraction('0.01', '0.02') 
                               ConditionOnColumn('stratum')
                               ConditionOn('0', '1'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName = 'public.batting_enh',
                                        sampleFraction = c(0.01,0.02), 
                                        conditionStratum = "yearid % 2", 
                                        conditionValues = c('0','1'),
                                        where = "lgid = 'NL'", test = TRUE),
                          "SELECT * 
                             FROM sample(
                               ON (SELECT  * , yearid % 2 as stratum FROM public.batting_enh WHERE lgid = 'NL' ) 
                               SampleFraction('0.01', '0.02') 
                               ConditionOnColumn('stratum')
                               ConditionOn('0', '1'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName = 'public.batting_enh',
                                        sampleFraction = c(0.01,0.02),
                                        include = c('decadeid','g','ab','r','h'),
                                        conditionStratum = "yearid % 2", 
                                        conditionValues = c('0','1'),
                                        where = "lgid = 'NL'", test = TRUE),
                          "SELECT * 
                             FROM sample(
                               ON (SELECT  decadeid, g, ab, r, h, yearid % 2 as stratum FROM public.batting_enh WHERE lgid = 'NL' ) 
                               SampleFraction('0.01', '0.02') 
                               ConditionOnColumn('stratum')
                               ConditionOn('0', '1'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName = 'public.batting_enh',
                                        sampleSize = 1000, 
                                        conditionStratum = "yearid % 2", 
                                        conditionValues = c('0','1'),
                                        test = TRUE),
                          "SELECT * 
                             FROM sample(
                               ON (SELECT  * , yearid % 2 as stratum  FROM public.batting_enh ) 
                                 AS DATA PARTITION BY ANY
                               ON (SELECT yearid % 2 as stratum, COUNT(*) as stratum_count FROM public.batting_enh GROUP BY 1) 
                                 AS SUMMARY DIMENSION
                               ApproximateSampleSize('1000') 
                               ConditionOnColumn('stratum')
                               ConditionOn('0', '1'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName = 'public.batting_enh',
                                        sampleSize = c(200, 200), 
                                        conditionStratum = "yearid % 2", 
                                        conditionValues = c('0','1'),
                                        test = TRUE),
                          "SELECT * 
                             FROM sample(
                               ON (SELECT  * , yearid % 2 as stratum  FROM public.batting_enh ) 
                                 AS DATA PARTITION BY ANY
                               ON (SELECT yearid % 2 as stratum, COUNT(*) as stratum_count FROM public.batting_enh GROUP BY 1) 
                                 AS SUMMARY DIMENSION
                               ApproximateSampleSize('200', '200') 
                               ConditionOnColumn('stratum')
                               ConditionOn('0', '1'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName = 'public.batting_enh',
                                        sampleSize = c(200, 200), 
                                        conditionStratum = "yearid % 2", 
                                        conditionValues = c('0','1'),
                                        where="lgid = 'NL'", test = TRUE),
                          "SELECT * 
                             FROM sample(
                               ON (SELECT  * , yearid % 2 as stratum FROM public.batting_enh WHERE lgid = 'NL' ) 
                                 AS DATA PARTITION BY ANY
                               ON (SELECT yearid % 2 as stratum, COUNT(*) as stratum_count 
                                     FROM public.batting_enh 
                                    WHERE lgid = 'NL' GROUP BY 1) 
                                 AS SUMMARY DIMENSION
                               ApproximateSampleSize('200', '200') 
                               ConditionOnColumn('stratum')
                               ConditionOn('0', '1'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName = 'public.batting_enh',
                                        sampleSize = c(200, 200), 
                                        include = c('decadeid','g','ab','r','h'),
                                        conditionStratum = "yearid % 2", 
                                        conditionValues = c('0','1'),
                                        where="lgid = 'NL'", test = TRUE),
                          "SELECT * 
                             FROM sample(
                               ON (SELECT  decadeid, g, ab, r, h, yearid % 2 as stratum FROM public.batting_enh WHERE lgid = 'NL' ) 
                                 AS DATA PARTITION BY ANY
                               ON (SELECT yearid % 2 as stratum, COUNT(*) as stratum_count 
                                     FROM public.batting_enh 
                                    WHERE lgid = 'NL' GROUP BY 1) 
                                 AS SUMMARY DIMENSION
                               ApproximateSampleSize('200', '200') 
                               ConditionOnColumn('stratum')
                               ConditionOn('0', '1'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName = 'public.batting_enh',
                                        sampleFraction = c(0.02, 0.04), 
                                        conditionStratum = "CASE yearid % 2 WHEN 0 THEN 'even'
                                                                           WHEN 1 THEN 'odd' 
                                                            END", 
                                        conditionValues = c('even','odd'),
                                        test = TRUE),
                          "SELECT * 
                             FROM sample(
                               ON (SELECT  * , CASE yearid % 2 WHEN 0 THEN 'even'
                                                                           WHEN 1 THEN 'odd' 
                                               END as stratum FROM public.batting_enh ) 
                               SampleFraction('0.02', '0.04') 
                               ConditionOnColumn('stratum')
                               ConditionOn('even', 'odd'))")
  
  expect_equal_normalized(computeSample(channel=NULL, tableName = 'public.batting_enh',
                                        sampleSize = c(200, 200), 
                                        conditionStratum = "CASE yearid % 2 WHEN 0 THEN 'even'
                                                                           WHEN 1 THEN 'odd' 
                                                           END", 
                                        conditionValues = c('even','odd'),
                                        test = TRUE),
                          "SELECT * 
                             FROM sample(
                               ON (SELECT  * , CASE yearid % 2 WHEN 0 THEN 'even'
                                                                           WHEN 1 THEN 'odd' 
                                                           END as stratum  FROM public.batting_enh ) 
                                 AS DATA PARTITION BY ANY
                               ON (SELECT CASE yearid % 2 WHEN 0 THEN 'even'
                                                                           WHEN 1 THEN 'odd' 
                                                           END as stratum, COUNT(*) as stratum_count FROM public.batting_enh GROUP BY 1) 
                                 AS SUMMARY DIMENSION
                               ApproximateSampleSize('200', '200') 
                               ConditionOnColumn('stratum')
                               ConditionOn('even', 'odd'))")
  
})