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
  
  expect_equal_normalized(compute(asterConn, "teams_enh",
                                  by = c("teamid", "decadeid"),
                                  aggregates = c("min(rank) minrank", "max(rank) maxrank"),
                                  where = "lgid = 'AL'",
                                  test = TRUE),
                          "SELECT teamid, decadeid, min(rank) minrank, max(rank) maxrank
                             FROM teams_enh
                            WHERE lgid = 'AL'
                            GROUP BY teamid, decadeid"
                          )
})