context("computeHistogram")

pitching_info = dget("pitchingInfo.dat")

test_that("computeHistogram throws errors", {
  
  expect_error(computeHistogram(channel=NULL),
               "Must provide table and column names.")
  
  expect_error(computeHistogram(channel=NULL, tableName="fake"),
               "Must provide table and column names.")
  
  expect_error(computeHistogram(channel=NULL, tableName="fake", columnName="nocolumn", test=TRUE),
               "Must provide tableInfo when test==TRUE")
  
  expect_error(computeHistogram(channel=NULL, tableName="pitching", columnName="nocolumn", tableInfo=pitching_info, test=TRUE),
               "No columns specified found in the table")
  
})


test_that("computeHistogram SQL is correct", {
  
  expect_equal_normalized(computeHistogram(channel=NULL, tableName="pitching", columnName="h",
                                           tableInfo=pitching_info, binMethod="manual", 
                                           test=TRUE),
                          "SELECT * FROM hist_reduce(
                                      ON hist_map(
                                           ON (SELECT   cast(h as numeric) h FROM pitching ) as data_input PARTITION BY ANY  
                                           binsize('6.3')
                                           startvalue('0')
                                           endvalue('189')    
                                           VALUE_COLUMN('h')     
                                      ) 
                                      partition by  1 )"
    )
  
  expect_equal_normalized(computeHistogram(channel=NULL, tableName="pitching", columnName="h",
                                             tableInfo=pitching_info, binMethod="manual", by="lgid",
                                             test=TRUE),
                          "SELECT * FROM hist_reduce(
                                      ON hist_map(
                                           ON (SELECT lgid, cast(h as numeric) h FROM pitching ) as data_input PARTITION BY ANY  
                                           binsize('6.3')
                                           startvalue('0')
                                           endvalue('189')    
                                           VALUE_COLUMN('h')     
                                           GROUP_COLUMNS('lgid')
                                      ) 
                                      partition by  lgid)"
  )
  
  expect_equal_normalized(computeHistogram(channel=NULL, tableName="pitching", columnName="h",
                                             tableInfo=pitching_info, binMethod="manual", by=c("lgid","teamid"),
                                             test=TRUE),
                          "SELECT * FROM hist_reduce(
                                      ON hist_map(
                                           ON (SELECT lgid, teamid, cast(h as numeric) h FROM pitching ) as data_input PARTITION BY ANY  
                                           binsize('6.3')
                                           startvalue('0')
                                           endvalue('189')    
                                           VALUE_COLUMN('h')     
                                           GROUP_COLUMNS('lgid', 'teamid')
                                      ) 
                                      partition by  lgid, teamid)"
  )
  
  expect_equal_normalized(computeHistogram(channel=NULL, tableName="pitching", columnName="h",
                                             tableInfo=pitching_info, binMethod="Sturges",
                                             test=TRUE),
                          "SELECT * FROM hist_reduce(
                                      ON hist_map(
                                           ON (SELECT cast(h as numeric) h FROM pitching ) as data_input PARTITION BY ANY
                                           ON hist_prep( ON (SELECT cast(h as numeric) h FROM pitching ) VALUE_COLUMN('h') ) as data_stat DIMENSION
                                           BIN_SELECT('Sturges')
                                           VALUE_COLUMN('h')
                                      )
                                      partition by 1 )"
  )
  
  expect_equal_normalized(computeHistogram(channel=NULL, tableName="pitching", columnName="h",
                                             tableInfo=pitching_info, binMethod="Sturges", by=c("lgid","teamid"),
                                             test=TRUE),
                          "SELECT * FROM hist_reduce(
                                      ON hist_map(
                                           ON (SELECT lgid, teamid, cast(h as numeric) h FROM pitching ) as data_input PARTITION BY ANY
                                           ON hist_prep( ON (SELECT cast(h as numeric) h FROM pitching ) VALUE_COLUMN('h') ) as data_stat DIMENSION
                                           BIN_SELECT('Sturges')
                                           VALUE_COLUMN('h')
                                           GROUP_COLUMNS('lgid', 'teamid')
                                      )
                                      partition by lgid, teamid)"
  )
  
  expect_equal_normalized(computeHistogram(channel=NULL, tableName="pitching", columnName="h",
                                             tableInfo=pitching_info, binMethod="Scott", by=c("lgid", "teamid"),
                                             where="yearid >= 1980",
                                             test=TRUE),
                          "SELECT * FROM hist_reduce(
                                      ON hist_map(
                                           ON (SELECT lgid, teamid, cast(h as numeric) h FROM pitching WHERE yearid >= 1980 ) as data_input PARTITION BY ANY
                                           ON hist_prep( ON (SELECT cast(h as numeric) h FROM pitching WHERE yearid >= 1980 ) VALUE_COLUMN('h') ) as data_stat DIMENSION
                                           BIN_SELECT('Scott')
                                           VALUE_COLUMN('h')
                                           GROUP_COLUMNS('lgid', 'teamid')
                                      )
                                      partition by lgid, teamid)"
  )
  
})