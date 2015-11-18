context("create Cluster Plots")

test_that("createCentroidPlot throws errors", {
  
  expect_error(createCentroidPlot(km$centroids, format="no-such-vis-format"),
               "'arg' should be one of \"line\", \"bar\", \"heatmap\", \"bar_dodge\"")
  
})