context("utils-failures")

pitching_info = dget("_pitchingInfo.dat")

test_that("exceptions are properly handled by utility functions", {
  
  expect_equal(isTable(NULL, NULL), logical(0))
  
  expect_equal(isTable(NULL, character(0)), logical(0))
  
  expect_null(viewTableSummary())
  
  expect_error(getTableSummary(NULL, tableName="abcdefg", parallel=TRUE),
               "Please register parallel backend appropriate to your platform to run with parallel=TRUE")
  
  expect_error(getTableSummary(NULL, tableName="fielding", mock=TRUE),
               "Test sql with 'getTableSummary' only for 'batting' or 'pitching' tables.")
  
  expect_error(getTableSummary(NULL, tableName="pitching", except=pitching_info$COLUMN_NAME, mock=TRUE), 
               "No columns specified found in the table 'pitching'")
  
  expect_error(getTableSummary(NULL, tableName="pitching", percentiles=-1, mock=TRUE),
               "Invalid percentile value\\(s\\) passed \\(below 0 or above 100\\): -1")
  
})


test_that("getNullCounts throws errors", {
  
  expect_error(getNullCounts(NULL, output='not-an-option'),
               ".*'arg' should be one of \"long\", \"wide\", \"matrix\".*")
  
  expect_error(getNullCounts(NULL),
               "Table name must be specified.")
  
  expect_error(getNullCounts(NULL, NULL, test=TRUE),
               "Must provide tableInfo when test==TRUE.")
  
  expect_error(getNullCounts(NULL, NULL),
               "first argument is not an open RODBC channel")
  
})


test_that("getNullCounts sql is correct", {
  
  expect_equal_normalized(getNullCounts(NULL, "pitching", include=c('lgid','yearid'), tableInfo = pitching_info, test=TRUE),
                          "select count(1) - count(lgid) as lgid, count(1) - count(yearid) as yearid from pitching  ")
  
  
})