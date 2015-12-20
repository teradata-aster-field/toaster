#' toaster: analytical and visualization toolbox for Teradata Aster 
#' 
#' toaster provides simple 2-step approach: compute in Aster - visualize and analyze in R.
#' Its `compute` functions use powerful in-database SQL statements and SQL/MR functions
#' running inside Aster's highly scalable parallel and distributed analytical platform. 
#' Then `create` functions visualize results with boxplots, scatterplots, histograms, 
#' heatmaps, word clouds, maps, or slope graphs.
#' 
#' @name toaster
#' @docType package
#' @import RODBC plyr reshape2 ggplot2 RColorBrewer wordcloud foreach
#' @importFrom scales muted 
#' @importFrom memoise memoise
#' @importFrom ggmap get_map geocode ggmap
NULL