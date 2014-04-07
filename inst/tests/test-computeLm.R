context("computeLm")

batting_info = dget("battingInfo.dat")

test_that("computeLm throws errors", {
  
  expect_error(computeLm(), 
               "Must provide connection.")
  
  expect_error(computeLm(channel=NULL), 
               "Must provide table and expression.")
  
  expect_error(computeLm(channel=NULL, tableName="fake_name"),
               "Must provide table and expression.")
  
  expect_error(computeLm(channel=NULL, tableName="fake_name", expr=y~1 , test=TRUE),
               "No predictors found in formula.")
  
  expect_error(computeLm(channel=NULL, tableName="fake_name", expr=~x, test=TRUE),
               "No predictors found in formula.")
})

test_that("computeLm SQL is correct", {
  
  expect_equal_normalized(
    computeLm(channel=NULL, tableName="batting", expr= ba ~ rbi, test=TRUE),
    "SELECT * 
       FROM linreg(
         ON linregmatrix(
           ON (SELECT rbi x1, ba y FROM batting ) 
         )
         PARTITION BY 1
       )")
  
  expect_equal_normalized(
    computeLm(channel=NULL, tableName="batting", expr= ba ~ rbi + bb + so, test=TRUE),
    "SELECT * 
       FROM linreg(
         ON linregmatrix(
           ON (SELECT rbi x1, bb x2, so x3, ba y FROM batting ) 
         )
         PARTITION BY 1
       )")
  
  expect_equal_normalized(
    computeLm(channel=NULL, tableName="batting", expr= ba ~ rbi + bb + so, 
                  where = "lgid = 'AL'", test=TRUE),
    "SELECT * 
       FROM linreg(
         ON linregmatrix(
           ON (SELECT rbi x1, bb x2, so x3, ba y FROM batting WHERE lgid = 'AL' ) 
         )
         PARTITION BY 1
       )")
  
  
})