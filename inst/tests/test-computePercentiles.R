context("computePercentiles")

test_that("computePercentiles throws errros", {
  
  expect_error(computePercentiles(channel=NULL),
               "Must provide table and column names.")
  
  expect_error(computePercentiles(channel=NULL, tableName="fake_table"),
               "Must provide table and column names.")
  
  expect_error(computePercentiles(channel=NULL, tableName=NULL),
               "Must provide table and column names.")
  
  expect_error(computePercentiles(channel=NULL, tableName="fake_table", columnName=NULL),
               "Must provide table and column names.")
})

test_that("computePercentiles SQL is correct", {
  
  expect_equal_normalized(
    computePercentiles(channel=NULL, tableName="pitching", columnName="ipouts",
                       test=TRUE),
    "SELECT * FROM approxPercentileReduce(
       ON (
         SELECT * FROM approxPercentileMap(
           ON  ( SELECT * FROM pitching  )                       
           TARGET_COLUMN( 'ipouts' )
           ERROR( 1 )                       
       ) )
       PARTITION BY  1                   
       PERCENTILE( 0,5,10,25,50,75,90,95,100 )
     )")
  
  expect_equal_normalized(
    computePercentiles(channel=NULL, tableName="pitching", columnName="ipouts",
                       by="lgid", test=TRUE),
    "SELECT * FROM approxPercentileReduce(
       ON (
         SELECT * FROM approxPercentileMap(
           ON  ( SELECT * FROM pitching  )                       
           TARGET_COLUMN( 'ipouts' )
           ERROR( 1 )  
           GROUP_COLUMNS( 'lgid' )
       ) )
       PARTITION BY lgid                  
       PERCENTILE( 0,5,10,25,50,75,90,95,100 ) 
       GROUP_COLUMNS( 'lgid' )                  
    )")
  
  expect_equal_normalized(
    computePercentiles(channel=NULL, tableName="pitching", columnName="ipouts",
                       percentiles = c(1,2,3,4),
                       test=TRUE),
    "SELECT * FROM approxPercentileReduce(
      ON (
        SELECT * FROM approxPercentileMap(
          ON  ( SELECT * FROM pitching  )                       
          TARGET_COLUMN( 'ipouts' )
          ERROR( 1 )                       
        ) )
      PARTITION BY  1                   
      PERCENTILE( 0,1,2,3,4,25,50,75,100 )
    )")
})
