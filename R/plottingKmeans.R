
#' Create plot of cluster centroids
#' 
#' Visualize centroids produced by clustering function like k-means.
#' Plots available are line plot, bar plot, or heatmap. Parameter \code{format}
#' specifies which one to create.
#' 
#' @param centroids Kmeans centroids returned by \code{computeKmeans}.
#' @param format type of plot to use: line, bar, bar_dodge, bar_facet (same as bar) or heatmap.
#' @param clusterId name of a column with cluster id.
#' @param baseSize \code{\link{theme}} base font size.
#' @param baseFamily \code{\link{theme}} base font family.
#' @param title plot title.
#' @param xlab a label for the x axis, defaults to a description of x.
#' @param ylab a label for the y axis, defaults to a description of y.
#' @param legendPosition the position of legends. ("left", "right", "bottom", "top", or two-element numeric 
#'   vector). "none" is no legend.
#' @param defaultTheme plot theme to use: \code{\link[ggthemes]{theme_tufte}} is default.
#' @param themeExtra any additional \code{\link[ggplot2]{theme}} settings that override default theme.
#' 
#' @return ggplot object
#' @export
#' @examples 
#' if(interactive()){
#' # initialize connection to Lahman baseball database in Aster 
#' conn = odbcDriverConnect(connection="driver={Aster ODBC Driver};
#'                          server=<dbhost>;port=2406;database=<dbname>;uid=<user>;pwd=<pw>")
#'                          
#' km = computeKmeans(conn, "batting", 
#'                    aggregates = c("COUNT(*) cnt", "AVG(g) avg_g", "AVG(r) avg_r", "AVG(h) avg_h"),
#'                    id="playerid || '-' || stint || '-' || teamid || '-' || yearid", 
#'                    include=c('g','r','h'), scaledTableName='kmeans_test_scaled', 
#'                    centroidTableName='kmeans_test_centroids', schema='baseball',
#'                    where="yearid > 2000", test=FALSE)
#' createCentroidPlot(km$centroids)
#' createCentroidPlot(km$centroids, format="bar_dodge")
#' createCentroidPlot(km$centroids, format="bar")
#' }
createCentroidPlot <- function(centroids, format=c('line'), clusterId="clusterid",
                               baseSize = 12, baseFamily = "serif",
                               title = paste("Cluster Centroids", format, "Plot"), 
                               xlab = ifelse(format=="line", "cluster", "variable"), 
                               ylab = ifelse(format=="heatmap", "cluster", "scaled value"), 
                               legendPosition = ifelse(format=="bar", "none", "right"),
                               defaultTheme=ggthemes::theme_tufte(base_size = baseSize, base_family = baseFamily, ticks=FALSE),
                                     themeExtra = NULL) {
  
  # match argument values
  format = match.arg(format, c('line','bar', 'heatmap','bar_dodge'))
  
  data = melt(centroids,id.vars=clusterId)
  
  if (format=='line') {
    p = plotLineCentroids(data, clusterId)
  }else if (format=='bar') {
    p = plotBarCentroids(data, clusterId)
  }else if (format=='bar_dodge') {
    p = plotBarDodgeCentroids(data, clusterId)
  }else {
    p = plotHeatmapCentroids(data, clusterId)
  }
  
  border_element = if(format=='bar') element_rect(fill=NA) else element_blank()
  
  p = p +
    labs(title=title, x=xlab, y=ylab) +
    defaultTheme + 
    theme(legend.position=legendPosition,
          panel.border = border_element) +
    themeExtra
  
  return(p)
}


# Barplot with facets
plotBarCentroids <- function(data, id) {
  
  facet_formula = stats::as.formula(paste("~", id))
  
  ggplot(data) +
    geom_bar(aes_string("variable", "value", fill=id), stat="identity", position="dodge") +
    # ggplol2 version post 1.0.1
    # facet_wrap(facet_formula, scales="fixed", dir="h", labeller=labeller(.default=cluster_labeller)) +
    facet_wrap(facet_formula, scales="fixed") +
    coord_flip() 
}

cluster_labeller <- function(value) {
  paste("Cluster", value)
}

# Barplot dodged
plotBarDodgeCentroids <- function(data,id) {
  
  ggplot(data) +
    geom_bar(aes_string("variable", "value", fill=id), 
             stat="identity", position="dodge", color="black") +
    coord_flip()
}

# Lineplot
plotLineCentroids <- function(data, id) {
  
  data[, id] = as.numeric(as.character(data[, id]))
  ggplot(data) +
    geom_line(aes_string(id, "value", color="variable")) +
    geom_point(aes_string(id, "value", color="variable"), size=3)
                
}

# Heatmp
plotHeatmapCentroids <- function(data, id) {
  
  ggplot(data) +
    geom_tile(aes_string("variable", id, fill="value")) +
    scale_fill_gradient2()
  
}


#' Create clusters' properties plot
#' 
#' @param aggregates lis of cluster properties
#' @param clusterId name of a column with cluster id.
#' @param baseSize \code{\link{theme}} base font size.
#' @param baseFamily \code{\link{theme}} base font family.
#' @param title plot title.
#' @param xlab a label for the x axis, defaults to a description of x.
#' @param ylab a label for the y axis, defaults to a description of y.
#' @param border boolean indicates to use border around plotting area. In case of facets border is around each facet.
#' @param defaultTheme plot theme to use: \code{\link[ggthemes]{theme_tufte}} is default.
#' @param themeExtra any additional \code{\link[ggplot2]{theme}} settings that override default theme.
#' 
#' @return ggplot object
#' @export
#' @examples 
#' if(interactive()){
#' # initialize connection to Lahman baseball database in Aster 
#' conn = odbcDriverConnect(connection="driver={Aster ODBC Driver};
#'                          server=<dbhost>;port=2406;database=<dbname>;uid=<user>;pwd=<pw>")
#'                          
#' km = computeKmeans(conn, "batting", 
#'                    aggregates = c("COUNT(*) cnt", "AVG(g) avg_g", "AVG(r) avg_r", "AVG(h) avg_h"),
#'                    id="playerid || '-' || stint || '-' || teamid || '-' || yearid", 
#'                    include=c('g','r','h'), scaledTableName='kmeans_test_scaled', 
#'                    centroidTableName='kmeans_test_centroids', schema='baseball',
#'                    where="yearid > 2000", test=FALSE)
#' createClusterPlot(km$aggregates)
#' }
createClusterPlot <- function(aggregates, clusterId="clusterid",
                              baseSize = 12, baseFamily = "serif",
                              title = paste("Cluster Properties Plot"), xlab = "cluster", ylab = "value", 
                              border=TRUE,
                              defaultTheme=ggthemes::theme_tufte(base_size = baseSize, base_family = baseFamily, ticks=FALSE),
                              themeExtra = NULL) {
  
  data = melt(aggregates,id.vars=clusterId)
  
  facet_formula = stats::as.formula(paste("~", "variable"))
  border_element = if(border) element_rect(fill=NA) else element_blank()
  
  p = ggplot(data) +
    geom_bar(aes_string(clusterId, "value", fill="variable"), stat="identity", position="dodge") +
    facet_wrap(facet_formula, scales="free") +
    # post ggplot2 1.0.1 version
    # facet_wrap(facet_formula, scales="free", dir="h", labeller=labeller(.default=agg_labeller)) +
    labs(title=title, x=xlab, y=ylab) +
    defaultTheme + 
    theme(legend.position="none",
          panel.border = border_element) +
    themeExtra
  
  return(p)
}

agg_labeller <- function(value) {
  paste("Property", value)
}


#' Create cluster variable plot
#' 
#' @param kms sample of points assigned to clusters
#' @param clusterId name of a column with cluster id.
#' @param baseSize \code{\link{theme}} base font size.
#' @param baseFamily \code{\link{theme}} base font family.
#' @param title plot title.
#' @param ticks \code{logical} Show axis ticks?
#' @param defaultTheme plot theme to use: \code{\link[ggthemes]{theme_tufte}} is default.
#' @param themeExtra any additional \code{\link[ggplot2]{theme}} settings that override default theme.
#' @param ... other parameters being suplied to geom's \code{aes}
#' 
#' @return ggplot object
#' @export
#' @examples 
#' if(interactive()){
#' # initialize connection to Lahman baseball database in Aster 
#' conn = odbcDriverConnect(connection="driver={Aster ODBC Driver};
#'                          server=<dbhost>;port=2406;database=<dbname>;uid=<user>;pwd=<pw>")
#'                          
#' km = computeKmeans(conn, "batting", 
#'                    id="playerid || '-' || stint || '-' || teamid || '-' || yearid", 
#'                    include=c('g','r','h'), scaledTableName='kmeans_test_scaled', 
#'                    centroidTableName='kmeans_test_centroids', schema='baseball',
#'                    where="yearid > 2000", test=FALSE)
#' kms = computeClusterSample(conn, km, 0.01, test=FALSE)
#' createClusterPairsPlot(kms, "Batters Clustered by G, R, H", ticks=FALSE)
#' }
createClusterPairsPlot <- function(kms, clusterId="clusterid", 
                                   title="Cluster Variable Pairs", 
                                   baseSize = 12, baseFamily = "serif", ticks=TRUE,
                                   defaultTheme=ggthemes::theme_tufte(base_size = baseSize, base_family = baseFamily, ticks = ticks),
                                   themeExtra = theme(), ...) {
  
  if (!is.factor(kms$clusterid)) 
    kms$clusterid = factor(kms$clusterid)
  
  p = GGally::ggpairs(kms, color='clusterid', title=title, ...) +
    defaultTheme +
    themeExtra
  
  return(p)
}