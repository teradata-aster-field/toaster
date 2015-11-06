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
  
  expect_error(getTableSummary(NULL, tableName="batting", percentiles=c(-1, 0, 2, 101, 99), mock=TRUE),
               "Invalid percentile value\\(s\\) passed \\(below 0 or above 100\\): -1Invalid percentile value\\(s\\) passed \\(below 0 or above 100\\): 101")
})