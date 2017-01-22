context("create Cluster Plots")

centers = matrix(c(1,1,1,1,1,
                   2,2,2,2,2,
                   3,3,3,3,3),
                 nrow=5,
                 dimnames = list(c("1","2","3","4","5"),c("f1","f2","f3")))
kmobj = structure(list(centers=centers,
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
  
  expect_error(createCentroidPlot(km=kmobj, clusters = c('1','2','6','12')),
               "All clusters must be defined in kmeans object: 6,12.")
  
   expect_error(createCentroidPlot(km=kmobj, clusters = c(name1='1',name2='2',name3='16',name4='-2')),
               "All clusters must be defined in kmeans object: 16,-2.")
  
  expect_error(createCentroidPlot(km=kmobj, dims = NULL),
               "All dimensions must be defined in kmeans object: .")
  
  expect_error(createCentroidPlot(km=kmobj, dims = c()),
               "All dimensions must be defined in kmeans object: .")
  
  expect_error(createCentroidPlot(km=kmobj, dims = c("f1","f6","f12")),
               "All dimensions must be defined in kmeans object: f6,f12.")
  
  expect_error(createCentroidPlot(km=kmobj, dims = c(f1name="f1", f12name="f12", "f3")),
               "All dimensions must be defined in kmeans object: f12.")
  
  expect_error(createClusterPlot(),
               "Kmeans object must be specified.")
  
  expect_error(createClusterPlot(km=structure(list(), class = c("toakmeans", "kmeans"))),
               "Kmeans object is missing cluster aggregates.")
  
  expect_error(createClusterPlot(km=kmobj, clusters = NULL),
               "All clusters must be defined in kmeans object: .")
  
  expect_error(createClusterPlot(km=kmobj, clusters = c()),
               "All clusters must be defined in kmeans object: .")
  
  expect_error(createClusterPlot(km=kmobj, clusters = c(1,10,11)),
               "All clusters must be defined in kmeans object: 10,11.")
  
  expect_error(createClusterPlot(km=kmobj, clusters = c('1','10','11')),
               "All clusters must be defined in kmeans object: 10,11.")
  
  expect_error(createClusterPlot(km=kmobj, clusters = c(cl1='1',cl2='101',cl3='111')),
               "All clusters must be defined in kmeans object: 101,111.")
  
  expect_error(createClusterPlot(km=kmobj, aggregates = NULL),
               "All aggregate properties must be defined in kmeans object: .")
  
  expect_error(createClusterPlot(km=kmobj, aggregates = c()),
               "All aggregate properties must be defined in kmeans object: .")
  
  expect_error(createClusterPlot(km=kmobj, aggregates = c("nosuch1","nosuch21","cnt")),
               "All aggregate properties must be defined in kmeans object: nosuch1,nosuch2.")
  
  expect_error(createClusterPairsPlot(),
               "Kmeans object must be specified.")
  
  expect_error(createClusterPairsPlot(km=structure(list(), class = c("toakmeans"))),
               "Kmeans object is missing sample data.")
  
  expect_error(createClusterPairsPlot(km=structure(list(centers=centers,
                                                        data=data.frame(col1=c(1,2,3), col2=c('a','b','c'))), 
                                                   class = c("toakmeans"), 
                                                   .Names = c("centers", "data")),
                                      include=c('nosuch1','nosuch2')),
               "No columns left to plot.")
  
  expect_error(createClusterPairsPlot(km=structure(list(centers=centers,
                                                        data=data.frame(col1=c(1,2,3), col2=c('a','b','c'))), 
                                                   class = c("toakmeans"), 
                                                   .Names = c("centers", "data")),
                                      except=c('col1','col2')),
               "No columns left to plot.")
  
  expect_error(createClusterPairsPlot(km=structure(list(data=data.frame(col1=c(1,2,3), col2=c('a','b','c'))), 
                                                   class = c("toakmeans"), 
                                                   .Names = c("data")),
                                      clusters = NULL),
               "All clusters must be defined in kmeans object: .")
  
  expect_error(createClusterPairsPlot(km=structure(list(data=data.frame(col1=c(1,2,3), col2=c('a','b','c'))), 
                                                   class = c("toakmeans"), 
                                                   .Names = c("data")),
                                      clusters = c()),
               "All clusters must be defined in kmeans object: .")
  
  kmobj$data=data.frame(col1=c(1,2,3), col2=c('a','b','c'))
  
  expect_error(createClusterPairsPlot(km=kmobj, clusters = c(1,10,11)),
               "All clusters must be defined in kmeans object: 10,11.")
  
  expect_error(createClusterPairsPlot(km=kmobj, clusters = c('1','10','11')),
               "All clusters must be defined in kmeans object: 10,11.")
  
  expect_error(createClusterPairsPlot(km=kmobj, clusters = c(cl1='1',cl2='101',cl3='111')),
               "All clusters must be defined in kmeans object: 101,111.")
  
  expect_error(createSilhouetteProfile(),
               "Kmeans object must be specified.")
  
  expect_error(createSilhouetteProfile(km=structure(list(), class = c("toakmeans", "kmeans"))),
               "Kmeans object is missing silhouette data.")
  
  expect_error(createSilhouetteProfile(km=structure(list(sil=list()), class = c("toakmeans", "kmeans"))),
               "Kmeans object is missing silhouette data.")
})