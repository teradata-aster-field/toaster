context("computePercentiles")

test_that("computePercentiles throws errros", {
  
  expect_error(computePercentiles(), 
               "Must provide connection.")
  
  expect_error(computePercentiles(channel=NULL),
               "Must provide table name.")
  
  expect_error(computePercentiles(channel=NULL, tableName=NULL),
               "Must provide table name.")
  
  expect_error(computePercentiles(channel=NULL, tableName="fake_table"),
               "Must provide at least one column name.")
  
  expect_error(computePercentiles(channel=NULL, tableName="fake_table", columnName=NULL),
               "Must provide at least one column name.")
  
  expect_error(computePercentiles(channel=NULL, tableName="fake_table", columnNames=NULL),
               "Must provide at least one column name.")
  
  expect_error(computePercentiles(channel=NULL, tableName="fake_table", columnNames=character(0)),
               "Must provide at least one column name.")
})

test_that("computePercentiles SQL is correct", {
  
  expect_equal_normalized(
    computePercentiles(channel=NULL, tableName="pitching", columnName="ipouts",
                       test=TRUE),
    "SELECT * FROM approxPercentileReduce(
       ON (
         SELECT * FROM approxPercentileMap(
           ON  ( SELECT * FROM pitching )                       
           TARGET_COLUMN( 'ipouts' )
           ERROR( 1 )                       
       ) )
       PARTITION BY  1                   
       PERCENTILE( 0,5,10,25,50,75,90,95,100 )
     )")
  
  expect_equal_normalized(
    computePercentiles(channel=NULL, tableName="pitching", columnNames="ipouts",
                       test=TRUE),
    "SELECT * FROM approxPercentileReduce(
       ON (
         SELECT * FROM approxPercentileMap(
           ON  ( SELECT * FROM pitching )                       
           TARGET_COLUMN( 'ipouts' )
           ERROR( 1 )                       
       ) )
       PARTITION BY  1                   
       PERCENTILE( 0,5,10,25,50,75,90,95,100 )
     )")
  
  expect_equal_normalized(
    computePercentiles(channel=NULL, tableName="pitching", columnNames="ipouts",
                       by="lgid", test=TRUE),
    "SELECT * FROM approxPercentileReduce(
       ON (
         SELECT * FROM approxPercentileMap(
           ON  ( SELECT * FROM pitching )                       
           TARGET_COLUMN( 'ipouts' )
           ERROR( 1 )  
           GROUP_COLUMNS( 'lgid' )
       ) )
       PARTITION BY lgid                  
       PERCENTILE( 0,5,10,25,50,75,90,95,100 ) 
       GROUP_COLUMNS( 'lgid' )                  
    )")
  
  expect_equal_normalized(
    computePercentiles(channel=NULL, tableName="pitching", columnNames="ipouts",
                       percentiles = c(1,2,3,4),
                       test=TRUE),
    "SELECT * FROM approxPercentileReduce(
      ON (
        SELECT * FROM approxPercentileMap(
          ON  ( SELECT * FROM pitching )                       
          TARGET_COLUMN( 'ipouts' )
          ERROR( 1 )                       
        ) )
      PARTITION BY  1                   
      PERCENTILE( 0,1,2,3,4,25,50,75,100 )
    )")
  
  expect_equal_normalized(
    computePercentiles(channel=NULL, tableName="pitching", columnNames="ipouts",
                       percentiles = c(10,90), where="lgid = 'NL'",
                       test=TRUE),
    "SELECT * FROM approxPercentileReduce(
      ON (
        SELECT * FROM approxPercentileMap(
          ON  ( SELECT * FROM pitching WHERE lgid = 'NL' )                       
          TARGET_COLUMN( 'ipouts' )
          ERROR( 1 )                       
        ) )
      PARTITION BY  1                   
      PERCENTILE( 0,10,25,50,75,90,100 )
    )")
  
  expect_equal_normalized(
    computePercentiles(channel=NULL, tableName="pitching_enh", columnNames=c("ipouts","era"),
                       by=c("lgid","decadeid"), where="lgid IN ('NL','AL')",
                       test=TRUE),
    "SELECT * FROM approxPercentileReduce(
       ON (
         SELECT * FROM approxPercentileMap(
           ON  ( SELECT * FROM pitching_enh WHERE lgid IN ('NL','AL') )                       
           TARGET_COLUMN( 'ipouts' )
           ERROR( 1 )  
           GROUP_COLUMNS( 'lgid', 'decadeid' )
       ) )
       PARTITION BY lgid, decadeid                  
       PERCENTILE( 0,5,10,25,50,75,90,95,100 ) 
       GROUP_COLUMNS( 'lgid', 'decadeid' )                  
    )")
})
