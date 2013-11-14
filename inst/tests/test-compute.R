context("compute")

source("test-utils.R")

test_that("compute throws errors", {
  
  expect_error(compute(channel=NULL), 
               "Must have table name.")
  
  expect_error(compute(channel=NULL, tableName="fake"),
               "Must have one or more columns.")
  
})


test_that("compute SQL is correct", {
  
  expect_equal_normalized(compute(asterConn, "teams_enh", 
                                  by = c("name || ', ' || park teamname", "lgid", "teamid", "decadeid"),
                                  aggregates = c("min(name) name", "min(park) park", "avg(rank) rank", 
                                                 "avg(attendance) attendance"),
                                  test = TRUE),
                          "SELECT name || ', ' || park teamname, lgid, teamid, decadeid, min(name) name, 
                                  min(park) park, avg(rank) rank, avg(attendance) attendance 
                             FROM teams_enh  
                            GROUP BY name || ', ' || park, lgid, teamid, decadeid"                          
                          )
})