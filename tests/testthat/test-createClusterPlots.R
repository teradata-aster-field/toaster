context("create Cluster Plots")

kmobj = structure(list(centers=matrix(c(1,1,1,1,1,
                                        2,2,2,2,2,
                                        3,3,3,3,3),nrow=5,
                                      dimnames = list(c("1","2","3","4","5"),c("f1","f2","f3"))),
                       aggregates=data.frame(clusterid="1","2","3","4","5", cnt=c(10,20,30,20,30),
                                             withinns=c(100,200,300,299,120), avg_a=c(0.1,0.2,0.3,0.2,0.1))),
                  class = c("toakmeans", "kmeans"))

test_that("Cluster plot functions throw errors", {
  
  expect_error(createCentroidPlot(km=structure(list(centers=matrix(c(1), nrow=1, byrow = TRUE)),
                                                    class = c("toakmeans", "kmeans")), 
                                  format="no-such-vis-format"),
               "'arg' should be one of \"line\", \"bar\", \"heatmap\", \"bar_dodge\"")
  
  expect_error(createCentroidPlot(),
               "Kmeans or canopy object must be specified.")
  
  expect_error(createCentroidPlot(km=1),
               "Kmeans or canopy object must be specified.")
  
  expect_error(createCentroidPlot(km=structure(list(), class = c("NOTtoakmeans", "NOTkmeans"))),
               "Kmeans or canopy object must be specified.")
  
  expect_error(createCentroidPlot(km=structure(list(), class = c("toakmeans", "kmeans"))),
               "Kmeans object is missing cluster centers.")
  
  expect_error(createCentroidPlot(km=kmobj, clusters = NULL),
               "All clusters must be defined in kmeans object: .")
  
  expect_error(createCentroidPlot(km=kmobj, clusters = c()),
               "All clusters must be defined in kmeans object: .")
  
  expect_error(createCentroidPlot(km=kmobj, clusters = c(1,2,6,12)),
               "All clusters must be defined in kmeans object: 6,12.")
  
  expect_error(createCentroidPlot(km=kmobj, dims = NULL),
               "All dimensions must be defined in kmeans object: .")
  
  expect_error(createCentroidPlot(km=kmobj, dims = c()),
               "All dimensions must be defined in kmeans object: .")
  
  expect_error(createCentroidPlot(km=kmobj, dims = c("f1","f6","f12")),
               "All dimensions must be defined in kmeans object: f6,f12.")
  
  expect_error(createClusterPlot(),
               "Kmeans object must be specified.")
  
  expect_error(createClusterPlot(km=structure(list(), class = c("toakmeans", "kmeans"))),
               "Kmeans object is missing cluster aggregates.")
  
  expect_error(createClusterPairsPlot(),
               "Kmeans object must be specified.")
  
  expect_error(createClusterPairsPlot(km=structure(list(), class = c("toakmeans"))),
               "Kmeans object is missing sample data.")
  
  expect_error(createClusterPairsPlot(km=structure(list(data=data.frame(col1=c(1,2,3), col2=c('a','b','c'))), 
                                                   class = c("toakmeans"), 
                                                   .Names = c("data")),
                                      include=c('nosuch1','nosuch2')),
               "No columns left to plot.")
  
  expect_error(createClusterPairsPlot(km=structure(list(data=data.frame(col1=c(1,2,3), col2=c('a','b','c'))), 
                                                   class = c("toakmeans"), 
                                                   .Names = c("data")),
                                      except=c('col1','col2')),
               "No columns left to plot.")
  
  expect_error(createSilhouetteProfile(),
               "Kmeans object must be specified.")
  
  expect_error(createSilhouetteProfile(km=structure(list(), class = c("toakmeans", "kmeans"))),
               "Kmeans object is missing silhouette data.")
  
  expect_error(createSilhouetteProfile(km=structure(list(sil=list()), class = c("toakmeans", "kmeans"))),
               "Kmeans object is missing silhouette data.")
})