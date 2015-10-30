context("createMap")

test_that("createMap throws errors", {
  
  expect_error(createMap(location=character(0)),
               "Parameter location is not numeric.")
  
  expect_error(createMap(location=numeric(0)),
               "Length of parameter location must be 2 or 4.")
  
  expect_error(createMap(location=c(19)),
               "Length of parameter location must be 2 or 4.")
  
  expect_error(createMap(location=c(20,21,22)),
               "Length of parameter location must be 2 or 4.")
               
  expect_error(createMap(location=c(20,21,22,24,25)),
                "Length of parameter location must be 2 or 4.")
})