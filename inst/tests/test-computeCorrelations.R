context("computeCorrelations")

test_that("computeCorrelations throws errors", {
  
  expect_error(computeCorrelations(test=TRUE),
               "Must provide tableInfo when test==TRUE")
  
})