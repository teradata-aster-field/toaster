require(ggplot2)
require(scales)
require(RColorBrewer)
require(reshape2)

#' Create plot with Heatmap visualization
#' 
#' @param thresholdValue threshold to use to display data in heatmap (if NULL then do not use threshold)
#' @param thresholdName name of data attribute from \code{data} to use (by defult use \code{fill})
#' @param text if TRUE then display values in heatmap table (default: FALSE) 
#' @param textFill text to display (applies only when \code{text} is TRUE), by defaul use \code{fill} values
#' @param percent format text as percent 
#' @param digits number of digits to use in text
#' @export
createHeatmap <- function(data, x, y, fill,
                          facet=NULL, ncol=1, baseSize=12, baseFamily="sans",
                          thresholdValue=NULL, thresholdName=fill,
                          text=FALSE, textFill=fill, percent=FALSE, digits=ifelse(percent, 2, 4),
                          title=paste("Heatmap by", fill), xlab=NULL, ylab=NULL,
                          lowGradient="white", highGradient="blue",
                          legend.position="right",
                          defaultTheme=theme_bw(base_size = baseSize),
                          themeExtra=NULL) {
  
  # Handle threshold if defined
  # must be first operation as it filters out data from being displayed
  if (!missing(thresholdValue)) {
    data = data[data[, thresholdName]>=thresholdValue,]
  }
  
  # Set text layer before ggplot 
#   l = setTextLayer(text, data, fill, percent, digits)
#   data = l$data
#   textLayer = l$textLayer
  textLayer = setTextLayer(text, data, x, y, textFill, position="identity", percent=percent, digits=digits)
  
  p = ggplot(data, aes_string(x=x, y=y)) +
    geom_tile(aes_string(fill=fill), colour = "white") +    
    scale_fill_gradient(low=lowGradient, high=highGradient) + 
    defaultTheme +
    labs(title=title, x=xlab, y=ylab) +
    #scale_x_discrete(expand = c(0, 0)) +
    theme(legend.position = legend.position, axis.ticks = element_blank(), 
          #axis.title.x = ifelse(missing(xlab), element_blank(), element_text()),
          #axis.title.y = ifelse(missing(ylab), element_blank(), element_text()),
          plot.title = element_text(family = baseFamily, face = "bold", size = baseSize * 1.4, vjust = 1),
          axis.text.x = element_text(size = baseSize * 0.8, 
                                     angle = 330, hjust = 0, colour = "grey50")) +
    textLayer +
    themeExtra
  
  if (!missing(facet)) {
    p = p + facet_wrap(as.formula(paste("~", facet)), ncol=ncol, scales="free_y") 
  }
  
  return(p)
}

#' Create plot with Histogram visualization
#' 
#' @param position histogram position parameter to use for overlapping bars: stack, dodge (defult), fill, identity 
#' @param mainColour Perimeter color of histogram bars
#' @param fillColour Fill color of histogram bars (applies if and only if \code{fill} is NULL)
#' @param scaleGradient Control \code{ggplot2} scale color gradient manually, e.g use \code{scale_colour_gradient}
#' @param palette Brewer palette name - see \code{display.brewer.all} in \code{RColorBrewer} package for names
#' @param facet Divide plot into facets for specificed parameter (defualt is NULL - no facets)
#' @param ncol Number of columns in facet wrap
#' @param facetScales Are scales shared across all facets: "fixed" - all are the same, "free_x" - vary across rows (x axis),
#'        "free_y" - vary across columns (Y axis) (default), "free" - both rows and columns (see in \code{facet_wrap} 
#'        parameter \code{scales} )
#' @param text if TRUE then display values above bars (default: FALSE) (this feature is in development)
#' @param defaultTheme plot theme to use, default is \code{theme_bw}
#' @param themeExtra any additional \code{ggplot2} theme attributes to add
#' @export
createHistogram <- function(data, x="bin_start", y="bin_count", fill=NULL, position="dodge", 
                            facet=NULL, ncol=1, facetScales="free_y", baseSize = 12, baseFamily="sans", 
                            xlim=NULL, breaks=NULL, 
                            text=FALSE, percent=FALSE, digits=0,
                            mainColour="black", fillColour="grey",
                            scaleGradient = NULL,
                            # TODO: imlement trendLines
                            trendLines=FALSE,  
                            title=paste("Histgoram by", fill), xlab=x, ylab=y, palette="Set1",
                            legend.position="right",
                            defaultTheme=theme_bw(base_size = baseSize),
                            themeExtra=NULL) { 
  
  # Set text layer before ggplot 
#   l = setTextLayer(text, data, y, percent, digits)
#   data = l$data
#   textLayer = l$textLayer
  textLayer = setTextLayerBin(text, data, x, y, y, fill=fill, position="identity", percent=percent, digits=digits)
  
  # make x discrete if it it is NOT
  if (!is.factor(data[,x])) {
    data[,x] = as.factor(data[,x])
  }
  
  # make facet discrete if it is NOT
  if (!missing(facet) & !is.factor(data[,facet])) {
    data[,facet] = as.factor(data[,facet])
  }
  
  p = ggplot(data) + 
    (if(missing(fill))
      geom_histogram(aes_string(x=x, y=y), colour=mainColour, fill=fillColour, stat="identity", position=position)
    else
      geom_histogram(aes_string(x=x, y=y, fill=fill), colour=mainColour, stat="identity", position=position)) +
    defaultTheme +
    labs(title=title, x=xlab, y=ylab) +
    theme(legend.position = legend.position, 
          axis.ticks = element_blank(), 
          #axis.title.x = element_blank(),
          #axis.title.y = element_blank(),
          plot.title = element_text(family = baseFamily, face = "bold", size = baseSize * 1.4, vjust = 1),
          axis.text.x = element_text(size = baseSize * 0.8, 
                                     angle = 330, hjust = 0, colour = "grey50")) +
    (if(missing(scaleGradient))
      scale_fill_brewer(palette = palette)
    else
      scaleGradient) +
    textLayer +
    themeExtra
  
  if (!missing(facet)) {
    p = p + facet_wrap(as.formula(paste("~", facet)), ncol=ncol, scales=facetScales)
  }
  
  if (!missing(xlim) | !missing(breaks)) {
    if (missing(xlim)) {
      xlim = levels(data[,x])
    }
    if (missing(breaks)) {
      breaks = levels(data[,x])
    }
    p = p + scale_x_discrete(limits=as.character(xlim), breaks=as.character(breaks))
  }
  
#   if (text) {
#     data$fill.value.text = paste0(prettyNum(data[, y], big.mark=",", digits=digits), ifelse(percent, "%", ""))
#     p = p + geom_text(aes(label=fill.value.text)) 
#   }
  
  if (trendLines) {
    p = p +
      geom_line(data=data, aes_string(x=x, y=y), colour="black")
  }
  
  return(p)
}

#' Create plot with Bubble Chart visualization 
#' 
#' @export
createBubblechart <- function(data, x, y, z, label=z, fill=NULL, 
                              facet=NULL, ncol=1, facetScales="fixed", xlim=NULL, baseSize=12, baseFamily="sans",
                              shape=21, scaleSizeRange=c(1,10),
                              title=paste("Bubble Chart by", fill), xlab=x, ylab=y, textSize = 4, textColour = "black",
                              legend.position="right",
                              defaultTheme=theme_bw(base_size = baseSize),
                              themeExtra=NULL) {
  
  p = ggplot(data, aes_string(x=x, y=y, size=z, label=label, fill=fill)) +
    geom_point(colour=textColour, shape=shape, show_guide=F) +
    geom_text(size=textSize, colour=textColour, vjust=1, show_guide=F) +
    scale_size_continuous(range=scaleSizeRange) +
    defaultTheme +
    labs(title=title, x=xlab, y=ylab) +
    theme(legend.position = legend.position,
          plot.title = element_text(family = baseFamily, face = "bold", size = baseSize * 1.4, vjust = 1)) +
    themeExtra
    
  if (!missing(facet)) {
    p = p + facet_wrap(as.formula(paste("~", facet)), ncol=ncol, scales=facetScales)
  }  
  
  if (!missing(xlim)) {
    p = p + xlim(xlim)
  }
  
  return (p)
  
}

#' Create plot with Slope Graph visualization
#' 
#'@export
createSlopegraph <- function(data, id, rankFrom, rankTo, 
                             reverse=TRUE, na.rm=FALSE, scaleFactor=1,
                             fromLabel=rankFrom, toLabel=rankTo,
                             title=paste("Slopegraph by", rankTo),
                             baseSize=12, baseFamily="sans",
                             classLabels=c(rankFrom, rankTo), classTextSize=12,
                             colour="#999999", upColour="#D55E00", downColour="#009E73", highlights=integer(0),
                             lineSize=0.15, textSize = 3.75, 
                             panelGridColour="black", panelGridSize=0.1) {
  
  if (na.rm) {
    data = data[complete.cases(data),]
  }
  
  data[, id] = reorder(data[, id], data[, rankTo])
  data$up_or_down = sign(data[,rankTo] - data[,rankFrom])
  datam = melt(data, id=c(id, fromLabel, toLabel, "up_or_down"), measure=c(rankFrom, rankTo))
  
  # convert to integers, scale and reverse order if necessary
  datam$valueScaled = scaleFactor * ifelse(reverse, -1, 1) * datam$value
  datam[,fromLabel] = as.integer(datam[,fromLabel])
  datam[,toLabel] = as.integer(datam[,toLabel])
  
  datam$fvariable = factor(datam$variable)
  datam$label_left = sprintf("%s %2d ", datam[, id] , datam[,fromLabel])
  datam$label_right = sprintf(" %-2d %s", datam[,toLabel], datam[, id])
  
  # Highlights
  sgPalette = rep(colour, length(datam[,id]))
  sgPalette[highlights] = ifelse(datam[highlights,"up_or_down"]>0, upColour, downColour)
  
  sg = ggplot(datam, aes_string(x="fvariable", y="valueScaled", 
                             group = id, 
                             colour = id, 
                             label = id)) +
    labs(title=title) +
    scale_x_discrete(labels=classLabels) +
    scale_colour_manual(values=sgPalette) +
    theme(legend.position = "none", 
          axis.text.x = element_text(size=classTextSize),
          axis.text.y=element_blank(), 
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks=element_blank(),
          axis.line=element_blank(),
          panel.grid.major = element_line(panelGridColour, size = panelGridSize),
          panel.grid.major = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(family = baseFamily, face = "bold", size = baseSize * 1.4, vjust = 1))
  
  # plot the right-most labels 
  sg1 = sg + geom_line(size=lineSize) + 
    geom_text(data = subset(datam, variable == rankTo), 
              aes(x = fvariable, label=label_right), size = textSize, hjust = 0) 
  
  # plot the left-most labels  
  sg1 = sg1 + geom_text(data = subset(datam, variable == rankFrom), 
                        aes(x = fvariable, label=label_left), size = textSize, hjust = 1)
  
  return(sg1)
  
}

#' Create plot with Word Cloud Visualization
#' 
#' Uses base graphics.
#' 
#' @param words the words
#' @param freq their frequencies
#' @param title plot title
#' @param scale a vector indicating the range of the size of the words (default c(4,.5))
#' @param filename file name to use where to save graphics
#' @param format format of graphics device to use
#' @export createWordcloud
createWordcloud <- function(words, freq, title="Wordcloud", 
                            scale=c(8,.2), minFreq=10, maxWords=40,
                            filename, format=c('png','bmp','jpeg','tiff','pdf'), width=480, height=480, units="px",
                            palette=brewer.pal(8,"Dark2"), textCex=1.) {
  
  # Obtain graphical device function
  gdevices = c('png','bmp','jpeg','tiff','pdf')
  if (!missing(filename) && !format %in% gdevices) 
    stop(cat("Graphic device '", format, "' wasn't found"))
  
  if (!missing(filename)) {
    gdev = get(format)
    gdev(filename, width=width, height=height, units=units, res=NULL)
  }
  
  layout(matrix(c(1,2), nrow=2), heights=c(1,7))
  par(mar=rep(0,4))
  plot.new()
  text(x=.5, y=.5, title, cex=textCex)
  wordcloud(words, freq, scale=scale, min.freq=minFreq,
            max.words=maxWords, random.order=FALSE, rot.per=.15, colors=palette, bg='transparent')
  
  if (!missing(filename)) {
    dev.off()
  }
  
}


# setTextLayer <- function(text, data, fill, percent, digits) {
# 
#   if (text) {
#     data$fill.value.text = paste0(prettyNum(data[, fill], big.mark=",", digits=digits), ifelse(percent, "%", ""))
#     textLayer = geom_text(aes(label=fill.value.text))
#     list(data=data, textLayer=textLayer)
#   }else {
#     list(data=data, textLayer=NULL)
#   }
# 
# }
setTextLayer <- function(text, data, x, y, value, position="dodge", 
                         percent, digits) {
  if (text) {
    data$.value.text = paste0(prettyNum(data[, value], big.mark=",", digits=digits), ifelse(percent, "%", ""))
    textLayer = geom_text(data=data, aes_string(x=x, y=y, label=".value.text"), position=position)
  }else {
    return (NULL)
  }
}

setTextLayerBin <- function(text, data, x, y, value, fill=NULL, position="dodge", size=12, hjust=0, vjust=0, 
                            percent, digits) {
  if (text) {
    data$.value.text = paste0(prettyNum(data[, value], big.mark=",", digits=digits), ifelse(percent, "%", ""))
    textLayer = stat_bin(data=data, geom="text", aes_string(x=x, y=y, label=".value.text", fill=fill), 
                         stat="identity", position=position, hjust=hjust, vjust=vjust)
  }else {
    return (NULL)
  }
}


#' Generate N color palette
#' 
#' inspired by 
#' http://stackoverflow.com/questions/13353213/gradient-of-n-colors-ranging-from-color-1-and-color-2
#' 
colorPalette <- function(n, colors=c("black", "white")) {
  colfunc = colorRampPalette(colors)
  colfunc(n)
}

#' Creates empty theme 
#' 
#' Good to use with slopegraphs
#' 
#' @export
theme_empty <- function (baseSize = 12, baseFamily = "") 
{
  theme_bw(base_size = baseSize, base_family = baseFamily) %+replace% 
    theme(axis.line = element_blank(),
          axis.text.x = element_text(family = baseFamily, 
                                     size = baseSize * 0.8, lineheight = 0.9, vjust = 1),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.length = unit(0, "lines"),
          axis.ticks.margin = unit(0, "lines"), 
          legend.background = element_rect(colour = NA),
          legend.key = element_rect(colour = "grey80"), 
          legend.key.size = unit(1.2, "lines"),
          legend.key.height = unit(NA, "cm"), 
          legend.key.width = unit(NA, "cm"),
          legend.text = element_text(family = baseFamily, 
                                     size = baseSize * 0.8),
          legend.text.align = 0, 
          legend.title = element_text(family = baseFamily,
                                      size = baseSize * 0.8, face = "bold", hjust = 0),
          legend.title.align = 0, 
          legend.position = "right",
          legend.direction = "vertical", 
          legend.box = 0,
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.margin = unit(0.25, "lines"), 
          strip.background = element_blank(),
          strip.text.x = element_text(family = baseFamily,
                                      size = baseSize * 0.8),
          strip.text.y = element_blank(),
          plot.background = element_blank(),
          plot.title = element_text(family = baseFamily,
                                    size = baseSize * 1.2),
          plot.margin = unit(c(1, 0.5, 0.5, 0.5), "lines"))
  
}