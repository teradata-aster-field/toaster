#' Create plot with Heatmap visualization
#' 
#' @param data data frame contains data computed for heatmap
#' @param x name of a column containing x variable values
#' @param y name of a column containing y variable values
#' @param fill name of a column with values to map to heatmap gradient colors (\code{lowGradient}, 
#'   \code{highGradient}, and optionally \code{midGradient}).
#' @param facet name of a column to divide plot into facets for specificed parameter (defualt is NULL - no facets). 
#'   If facet is single value then facet wrap applied (see \code{\link{facet_wrap}}), otherwise facet grid (see 
#'   \code{\link{facet_grid}} with 1st 2 values of the vector.
#' @param ncol number of facet columns (applies when single facet column supplied only - see parameter \code{facet}).
#' @param baseSize base font size
#' @param baseFamily base font family
#' @param thresholdValue threshold to use to display data in heatmap (if NULL then do not use threshold)
#' @param thresholdName name of data attribute from \code{data} to use (by defult use \code{fill})
#' @param text if TRUE then display values in heatmap table (default: FALSE) 
#' @param textFill text to display (applies only when \code{text} is TRUE), by defaul use \code{fill} values
#' @param percent format text as percent 
#' @param digits number of digits to use in text
#' @param lowGradient colour for low end of gradient.
#' @param midGradient colour for mid point.
#' @param highGradient colour for high end of gradient.
#' @param divergingColourGradient logical diverging colour gradient places emphasize on both low and high leaving
#'   middle neutral. Use when both end grandient colours represent critical values such as negative and positive 
#'   extremes (e.g. temprature, outliers, etc.)
#' @param title plot title
#' @param xlab a label for the x axis, defaults to a description of x.
#' @param ylab a label for the y axis, defaults to a description of y.
#' @param legendPosition the position of legends. ("left", "right", "bottom", "top", or two-element numeric 
#'   vector). "none" is no legend.
#' @param defaultTheme plot theme to use, default is \code{theme_bw}
#' @param themeExtra any additional \code{ggplot2} theme attributes to add
#' 
#' @export
createHeatmap <- function(data, x, y, fill,
                          facet = NULL, ncol = 1, baseSize = 12, baseFamily = "sans",
                          thresholdValue = NULL, thresholdName = fill,
                          text = FALSE, textFill = fill, percent = FALSE, digits = ifelse(percent, 2, 4),
                          divergingColourGradient = FALSE,
                          lowGradient = ifelse(divergingColourGradient, muted("red"), "#56B1F7"), 
                          midGradient = "white",
                          highGradient = ifelse(divergingColourGradient, muted("blue"), "#132B43"), 
                          title = paste("Heatmap by", fill), xlab = x, ylab = y,
                          legendPosition = "right",
                          defaultTheme = theme_bw(base_size = baseSize),
                          themeExtra = NULL) {
  
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
    (if (divergingColourGradient)
      scale_fill_gradient2(low=lowGradient, mid=midGradient, high=highGradient)
     else
       scale_fill_gradient(low=lowGradient, high=highGradient)
    ) + 
    defaultTheme +
    labs(title=title, x=xlab, y=ylab) +
    #scale_x_discrete(expand = c(0, 0)) +
    theme(legend.position = legendPosition, 
          axis.ticks = element_blank(), 
          #axis.title.x = ifelse(missing(xlab), element_blank(), element_text()),
          #axis.title.y = ifelse(missing(ylab), element_blank(), element_text()),
          plot.title = element_text(family = baseFamily, face = "bold", size = baseSize * 1.4, vjust = 1),
          axis.text.x = element_text(size = baseSize * 0.8, 
                                     angle = 330, hjust = 0, colour = "grey50")) +
    textLayer +
    themeExtra
  
  p = applyFacet(p, facet, "free_y", ncol)
  
  return(p)
}

#' Create histogram type of plot
#' 
#' Create histogram or bar chart plot.
#' 
#' @param data data frame contains computed histogram
#' @param x name of a column containing bin labels or interval values
#' @param y name of a column containing bin values or counts (bin size)
#' @param fill name of a column with values to colour bars
#' @param position histogram position parameter to use for overlapping bars: 
#'   stack, dodge (defult), fill, identity 
#' @param mainColour Perimeter color of histogram bars
#' @param fillColour Fill color of histogram bars (applies only when 
#'   \code{fill} is NULL)
#' @param scaleGradient control \code{ggplot2} scale fill gradient manually, 
#'   e.g use \code{scale_colour_gradient} (if specified then parameter 
#'   \code{palette} is ignored)
#' @param paletteValues actual palette colours for use with \code{scale_fill_manual}
#'  (if specified then parameter \code{palette} is ignored)
#' @param palette Brewer palette name - see \code{display.brewer.all} in 
#'   \code{RColorBrewer} package for names
#' @param facet name of a column to divide plot into facets for specificed parameter 
#'   (defualt is NULL - no facets). If facet is single value then facet wrap applied 
#'   (see \code{\link{facet_wrap}}), otherwise facet grid (see \code{\link{facet_grid}} 
#'   with 1st 2 values of the vector.
#' @param ncol number of facet columns (applies when single facet column supplied only 
#'   - see parameter \code{facet}).
#' @param facetScales Are scales shared across all facets: "fixed" - all are the same, 
#'   "free_x" - vary across rows (x axis), "free_y" - vary across columns (Y axis) 
#'   (default), "free" - both rows and columns (see in \code{facet_wrap} parameter
#'   \code{scales} )
#' @param baseSize base font size
#' @param baseFamily base font family
#' @param xlim a character vector specifying the data range for the x scale and the default order of their display 
#'   in the x axis.
#' @param breaks a character vector giving the breaks as they should appear on the x axis.
#' @param text if TRUE then display values above bars (default: FALSE) (this feature is in development)
#' @param percent format text as percent 
#' @param digits number of digits to use in text
#' @param textVJust vertical justificaiton of text labels (relative to the top of bar).
#' @param trend logical indicates if trend line is shown.
#' @param trendLinetype trend line type 
#' @param trendLinesize size of trend line
#' @param trendLinecolour color of trend line
#' @param title plot title
#' @param xlab a label for the x axis, defaults to a description of x.
#' @param ylab a label for the y axis, defaults to a description of y.
#' @param legendPosition the position of legends. ("left", "right", "bottom", "top", or two-element numeric 
#'   vector). "none" is no legend.
#' @param coordFlip logical flipped cartesian coordinates so that horizontal becomes vertical, and vertical horizontal (see 
#'   \link{coord_flip}).
#' @param defaultTheme plot theme to use, default is \code{\link{theme_bw}}
#' @param themeExtra any additional \code{ggplot2} theme attributes to add
#' 
#' @export
#' 
#' @examples
#' \donttest{
#' # AL teams pitching stats by decade
#' bc = computeBarchart(channel=conn, tableName="pitching_enh", category="teamid", 
#'                      aggregates=c("AVG(era) era", "AVG(whip) whip", "AVG(ktobb) ktobb"),
#'                      where="yearid >= 1990 and lgid='AL'", by="decadeid", withMelt=TRUE)
#' 
#' createHistogram(bc, "teamid", "value", fill="teamid", 
#'                 facet=c("variable", "decadeid"), 
#'                 legendPosition="bottom",
#'                 title = "AL Teams Pitching Stats by decades (1990-2012)",
#'                 themeExtra = guides(fill=guide_legend(nrow=2)))
#'
#' # AL Teams Average Win-Loss Difference by Decade 
#' franchwl = computeBarchart(conn, "teams_enh", "franchid",
#'                            aggregates=c("AVG(w) w", "AVG(l) l", "AVG(w-l) wl"),
#'                            by="decadeid",
#'                            where="yearid >=1960 and lgid = 'AL'")
#'
#' createHistogram(franchwl, "decadeid", "wl", fill="franchid",
#'                 facet="franchid", ncol=5, facetScales="fixed",
#'                 legendPosition="none",
#'                 trend=TRUE,
#'                 title="Average W-L difference by decade per team (AL)",
#'                 ylab="Average W-L")  
#'                 
#' # Histogram of team ERA distribution: Rangers vs. Yankees in 2000s
#' h2000s = computeHistogram(channel=conn, tableName='pitching_enh', columnName='era',
#'                           binsize=0.2, startvalue=0, endvalue=10, by='teamid',
#'                           where="yearID between 2000 and 2012 and teamid in ('NYA','TEX')")
#' createHistogram(h2000s, fill='teamid', facet='teamid', 
#'                 title='TEX vs. NYY 2000-2012', xlab='ERA', ylab='count',
#'                 legendPosition='none')                
#'                 
#' }
createHistogram <- function(data, x="bin_start", y="bin_count", fill=NULL, position="dodge", 
                            facet = NULL, ncol = 1, facetScales = "free_y", 
                            baseSize = 12, baseFamily = "sans", 
                            xlim = NULL, breaks = NULL, 
                            text = FALSE, percent = FALSE, digits = 0, textVJust = -2,
                            mainColour = "black", fillColour = "grey", 
                            scaleGradient = NULL, paletteValues = NULL, palette = "Set1", 
                            trend = FALSE, trendLinetype = "solid", trendLinesize = 1,
                            trendLinecolour = "black",
                            title = paste("Histgoram by", fill), xlab = x, ylab = y, 
                            legendPosition = "right",
                            coordFlip = FALSE,
                            defaultTheme=theme_bw(base_size = baseSize),
                            themeExtra = NULL) { 
  
  # Set text layer before ggplot 
  #   l = setTextLayer(text, data, y, percent, digits)
  #   data = l$data
  #   textLayer = l$
  # #another alternative:#
  # textLayer = setTextLayerBin(text, data, x, y, y, fill=fill, position="identity", percent=percent, digits=digits)
  if (text) {
    data$.value.text = paste0(prettyNum(data[, y], big.mark=",", digits=digits), ifelse(percent, "%", ""))
    textLayer = geom_text(aes_string(x=x, y=y, label=".value.text", fill=fill),
                          stat="identity", position="identity", vjust=textVJust)
  }else {
    textLayer = NULL
  }
  
  
  
  # make x discrete if it it is NOT
  if (!is.factor(data[,x])) {
    data[,x] = as.factor(data[,x])
  }
  
  # make facet discrete if it is NOT
  if (!missing(facet) & !is.factor(data[,facet])) {
    data[,facet[[1]]] = as.factor(data[,facet[[1]]])
    if (length(facet)>1) {
      data[,facet[[2]]] = as.factor(data[,facet[[2]]])
    }
  }
  
  p = ggplot(data) + 
    (if(missing(fill))
      geom_bar(aes_string(x=x, y=y), colour=mainColour, fill=fillColour, stat="identity", position=position)
    else
      geom_bar(aes_string(x=x, y=y, fill=fill), colour=mainColour, stat="identity", position=position)) +
    defaultTheme +
    labs(title=title, x=xlab, y=ylab) +
    theme(legend.position = legendPosition, 
          axis.ticks = element_blank(), 
          #axis.title.x = element_blank(),
          #axis.title.y = element_blank(),
          plot.title = element_text(family = baseFamily, face = "bold", size = baseSize * 1.4, vjust = 1),
          axis.text.x = element_text(size = baseSize * 0.8, 
                                     angle = 330, hjust = 0, colour = "grey50")) +
    (if(!missing(scaleGradient))
      scaleGradient
     else if(!missing(fill))
       # scale_fill_brewer(palette = palette)
       # scale_fill_discrete(palette = colorDiscretePalette(palette)) - won't work with legend 
       scale_fill_manual(values = (colorDiscretePalette(palette))(length(unique(data[,fill]))))
     else if(!missing(paletteValues)) 
       scale_fill_manual(values = paletteValues)
    ) +
    textLayer +
    themeExtra
  
  p = applyFacet(p, facet, facetScales, ncol)
  
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
  
  if (trend) {
    dataTrend = data
    dataTrend[, "X_trend_X"] = as.numeric(data[, x])
    p = p +
      geom_line(data=dataTrend, aes_string(x="X_trend_X", y=y), linetype=trendLinetype, size=trendLinesize, 
                colour=trendLinecolour)
  }
  
  if (coordFlip)
    p = p + coord_flip()
  
  return(p)
}

#' Create Bubble Chart plot
#' 
#' 
#' 
#' @param data data frame contains data computed for bubblechart
#' @param x name of a column containing x variable values
#' @param y name of a column containing y variable values
#' @param z name of a column containing bubble size value
#' @param label name of a column containing bubble label
#' @param fill name of a column with values to use for bubble colours
#' @param facet name of a column to divide plot into facets for specificed parameter (defualt is NULL - no facets). 
#'   If facet is single value then facet wrap applied (see \code{\link{facet_wrap}}), otherwise facet grid (see 
#'   \code{\link{facet_grid}} with 1st 2 values of the vector.
#' @param ncol number of facet columns (applies when single facet column supplied only - see parameter \code{facet}). 
#' @param facetScales Are scales shared across all facets: "fixed" - all are the same, "free_x" - vary across rows (x axis),
#'        "free_y" - vary across columns (Y axis) (default), "free" - both rows and columns (see in \code{facet_wrap} 
#'        parameter \code{scales} )
#' @param xlim a vector specifying the data range for the x scale and the default order of their display 
#'   in the x axis.        
#' @param baseSize base font size
#' @param baseFamily base font family
#' @param shape bubble shape
#' @param shapeColour colour of shapes
#' @param scaleSize logical if TRUE then scale the size of shape to be proportional to the value, 
#'   if FALSE then scale the area.
#' @param shapeSizeRange bubble size range (applies only when \code{scaleSize = TRUE})
#' @param shapeMaxSize size of largest shape (applies only when \code{scaleSize = FALSE})
#' @param paletteValues actual palette colours for use with \code{scale_fill_manual} (if specified then parameter
#'  \code{palette} is ignored)
#' @param palette Brewer palette name - see \code{display.brewer.all} in \code{RColorBrewer} package for names
#' @param title plot title
#' @param xlab a label for the x axis, defaults to a description of x.
#' @param ylab a label for the y axis, defaults to a description of y.
#' @param labelSize size of labels
#' @param labelFamily label font name or family name
#' @param labelFontface label font face (\code{c("plain","bold","italic","bold.italic")})
#' @param labelColour color of labels
#' @param labelVJust position of the anchor (0=bottom edge, 1=top edge), can go below 0 or above 1
#' @param labelHJust position of the label anchor (0=left edge, 1=right edge), can go below 0 or above 1
#' @param labelAlpha the transparency of the text label
#' @param labelAngle the angle at which to draw the text label
#' @param legendPosition the position of legends. ("left", "right", "bottom", "top", or two-element numeric 
#'   vector). "none" is no legend.
#' @param defaultTheme plot theme to use, default is \code{theme_bw}
#' @param themeExtra any additional \code{ggplot2} theme attributes to add
#' 
#' @export
createBubblechart <- function(data, x, y, z, label = z, fill = NULL, 
                              facet = NULL, ncol = 1, facetScales = "fixed",
                              xlim = NULL, baseSize = 12, baseFamily = "sans",
                              shape = 21, shapeColour = "black",
                              scaleSize = TRUE, shapeSizeRange = c(3,10), shapeMaxSize = 100,
                              paletteValues = NULL, palette = "Set1",
                              title = paste("Bubble Chart by", fill), xlab = x, ylab = y,
                              labelSize = 5, labelFamily = "", labelFontface = "plain",
                              labelColour = "black", labelVJust = 0.5, labelHJust = 0.5,
                              labelAlpha = 1, labelAngle = 0, 
                              legendPosition="right",
                              defaultTheme=theme_bw(base_size = baseSize),
                              themeExtra=NULL) {
  
  p = ggplot(data, aes_string(x=x, y=y, size=z, label=label, fill=fill), guide=F) +
    geom_point(colour=shapeColour, shape=shape, show_guide=T) +
    (if (!is.null(label)) 
      geom_text(size=labelSize, colour=labelColour, family=labelFamily, fontface=labelFontface, 
                vjust=labelVJust, hjust=labelHJust, angle=labelAngle, alpha=labelAlpha, 
                show_guide=F)) +
    (if (scaleSize)
       scale_size_continuous(range=shapeSizeRange, guide=FALSE)
     else
       scale_size_area(max_size=shapeMaxSize, guide=FALSE)) +
    (if(!missing(fill))
      if(!missing(paletteValues))
        scale_fill_manual(values = paletteValues)
      else if(!missing(palette))
        scale_fill_manual(values = (colorDiscretePalette(palette))(length(unique(data[,fill]))))
    ) +
    defaultTheme +
    labs(title=title, x=xlab, y=ylab) +
    theme(legend.position = legendPosition,
          plot.title = element_text(family = baseFamily, face = "bold", size = baseSize * 1.4, vjust = 1),
          axis.text.x = element_text(size = baseSize * 0.8, 
                                     angle = 330, hjust = 0),
          axis.text.y = element_text(size = baseSize * 0.8)) +
    themeExtra 
  
  p = applyFacet(p, facet, facetScales, ncol)
  
  if (!missing(xlim)) {
    p = p + xlim(xlim)
  }
  
  return(p)
  
}

#' Create plot with Slope Graph visualization
#' 
#' @param data data frame contains data computed for slopegraph
#' @param id name of column identifying each graph element having from (before) and to (after) pair of values
#' @param rankFrom name of column with from (before) value
#' @param rankTo name of column with to (after) value
#' @param reverse logical reverse values if TRUE (smaller is better)
#' @param na.rm logical value indicating whether NA values should be stripped before the visualization 
#'   proceeds.
#' @param scaleFactor scale factor applied to all values (-1 can be used instead of \code{reverse} TRUE).
#' @param fromLabel label for left values (from or before)
#' @param toLabel label for right values (to or after)
#' @param classLabels pair of labels for to and from columns (or classes)
#' @param classTextSize size of text for class labels
#' @param colour default colour
#' @param upColour colour of up slope
#' @param downColour colour of down slope
#' @param highlights vector with indexes of highlighted points
#' @param lineSize size of slope lines
#' @param textSize size of text
#' @param panelGridColour background panel grid colour
#' @param panelGridSize background panel grid size
#' @param title plot title
#' @param baseSize base font size
#' @param baseFamily base font family
#' @param defaultTheme plot theme to use, default is \code{theme_bw}
#' @param themeExtra any additional \code{ggplot2} theme attributes to add
#' 
#' @export
createSlopegraph <- function(data, id, rankFrom, rankTo, 
                             reverse = TRUE, na.rm = FALSE, scaleFactor = 1,
                             fromLabel = rankFrom, toLabel=rankTo,
                             title = paste("Slopegraph by", rankTo),
                             baseSize = 12, baseFamily = "sans",
                             classLabels = c(rankFrom, rankTo), classTextSize = 12,
                             colour = "#999999", upColour = "#D55E00", downColour = "#009E73", 
                             highlights = integer(0),
                             lineSize = 0.15, textSize = 3.75, 
                             panelGridColour = "black", panelGridSize = 0.1,
                             defaultTheme = theme_bw(base_size = baseSize),
                             themeExtra = NULL
) {
  
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
    defaultTheme +
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
          plot.title = element_text(family = baseFamily, face = "bold", size = baseSize * 1.4, vjust = 1)) +
    themeExtra
  
  # plot the right-most labels 
  dataTo = with(datam, datam[variable == rankTo, ])
  sg1 = sg + geom_line(size=lineSize) + 
    geom_text(data = dataTo,
              aes_string(x = 'fvariable', label='label_right'), size = textSize, hjust = 0) 
  
  # plot the left-most labels  
  dataFrom = with(datam, datam[variable == rankFrom, ])
  sg1 = sg1 + 
    geom_text(data = dataFrom,
              aes_string(x = 'fvariable', label='label_left'), size = textSize, hjust = 1)
  
  return(sg1)
  
}

#' Word Cloud Visual
#' 
#' Creates graphics with Word Cloud visual for text data.
#' 
#' @details
#' Uses base graphics and worldcloud package to create a word cloud (tag cloud) visual reprsentation of
#' for text data. Function uses 2 vectors of equal lengths: one contains list of words and the other has 
#' their frequencies. 
#' 
#' Resulting graphics is saved in file in one of available graphical formats (png, bmp, jpeg, tiff,
#' or pdf). 
#' 
#' Word Cloud visuals apply to any concept that satisfies following conditions:
#'  * each data point (artifact) can be expressed with distinct word or compact text in  distinct and 
#'    self-explanatory fashion and 
#'  * it assigns each artifact scalar non-negative metric.
#' Given these two conditions we can use Word Clouds to visualize top, bottom or all artifacts in single
#' word cloud visual.
#' 
#' @param words the words
#' @param freq their frequencies
#' @param title plot title
#' @param scale a vector indicating the range of the size of the words (default c(4,.5))
#' @param minFreq words with frequency below \code{minFreq} will not be displayed
#' @param maxWords Maximum number of words to be plotted (least frequent terms dropped).
#' @param filename file name to use where to save graphics
#' @param format format of graphics device to save wordcloud image
#' @param width the width of the output graphics device
#' @param height the height of the output graphics device
#' @param units the units in which \code{height} and \code{width} are given. Cab be \code{px} (pixels, 
#'   the default), \code{in} (inches), \code{cm} or \code{mm}.
#' @param palette color words from least to most frequent
#' @param titleFactor numeric title character expansion factor; multiplied by \code{\link{par}("cex")} 
#'   yields the final title character size. NULL and NA are equivalent to a factor of 1.
#' 
#' @return nothing
#' 
#' @seealso \link{wordcloud}
#' 
#' @export createWordcloud
createWordcloud <- function(words, freq, title="Wordcloud", 
                            scale=c(8,.2), minFreq=10, maxWords=40,
                            filename, format=c('png','bmp','jpeg','tiff','pdf'), 
                            width=480, height=480, units="px",
                            palette=brewer.pal(8,"Dark2"), titleFactor=1) {
  
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
  text(x=.5, y=.5, title, cex=titleFactor)
  wordcloud(words, freq, scale=scale, min.freq=minFreq,
            max.words=maxWords, random.order=FALSE, rot.per=.15, colors=palette, bg='transparent')
  
  if (!missing(filename)) {
    dev.off()
  }
  
}


#' Create plot with Population Pyramid visualization
#' 
#' Create population pyramid type of visual: two back-to-back bar graphs on the same 
#' category class (e.g. age) placed on Y-axis and distribution (population) placed 
#' on the X-axis. Bar graphs correspond to two distinct groups, e.g. sex (male
#' and female), baseball leagues (AL and NL), or customer types (new customers and 
#' established customers).
#' 
#' @param data data frame contains 2 histograms for the same bins. Bins are divided into 2 sets with
#'   parameter \code{divideBy}.
#' @param bin name of a column containing bin labels or interval values
#' @param count name of a column containing bin values or counts (bin size)
#' @param divideBy name of the column to divide data into two histograms
#' @param values two-valued vector containing values in \code{divideBy} (optional). If missing then it
#'   uses 1st 2 values from column \code{divideBy} (sorted with default order).
#' @param fillColours 2-value vector with colours for left and right histograms.
#' @param mainColour histogram bar colour. 
#' @param facet name of a column to divide plot into facets for specificed parameter (defualt is NULL - no facets). 
#'   If facet is single value then facet wrap applied (see \code{\link{facet_wrap}}), otherwise facet grid (see 
#'   \code{\link{facet_grid}} with 1st 2 values of the vector.
#' @param ncol number of facet columns (applies when single facet column supplied only - see parameter \code{facet}). 
#' @param facetScales Are scales shared across all facets: "fixed" - all are the same, "free_x" - vary across rows (x axis),
#'        "free_y" - vary across columns (Y axis) (default), "free" - both rows and columns (see in \code{facet_wrap} 
#'        parameter \code{scales} )
#' @param baseSize base font size
#' @param baseFamily base font family
#' @param title plot title.
#' @param xlab a label for the x axis, defaults to a description of x.
#' @param ylab a label for the y axis, defaults to a description of y.
#' @param legendPosition the position of legends. ("left", "right", "bottom", "top", or two-element numeric 
#'   vector). "none" is no legend.
#' @param defaultTheme plot theme to use, default is \code{theme_bw}
#' @param themeExtra any additional \code{ggplot2} theme attributes to add
#' 
#' @export 
#' @examples
#' \donttest{
#' pitchingInfo = getTableSummary(asterConn, tableName='pitching', where='yearid between 2000 and 2013')
#' battingInfo = getTableSummary(asterConn, tableName='batting', where='yearid between 2000 and 2013')
#'
#' salaryHistAll = computeHistogram(asterConn, tableName='public.salaries', columnName='salary', 
#'                                 binsize=200000, startvalue=0, 
#'                                 by='lgid', where='yearID between 2000 and 2013')
#' createPopPyramid(data=salaryHistAll, bin='bin_start', count='bin_count', divideBy='lgid', values=c('NL','AL'),
#'                  title="Salary Pyramid by MLB Leagues", xlab='Salary', ylab='Player Count')
#'
#' salaryHist5Mil = computeHistogram(asterConn, tableName='salaries', columnName='salary', 
#'                                   binsize=100000, startvalue=0, endvalue=5000000,
#'                                   by='lgid', where='yearID between 2000 and 2013')
#' createPopPyramid(data=salaryHist5Mil, divideBy='lgid', values=c('NL','AL'),
#'                  title="Salary Pyramid by MLB Leagues (less 5M only)", xlab='Salary', ylab='Player Count')
#'
#' eraHist = computeHistogram(asterConn, tableName='pitching', columnName='era', binsize=.1, startvalue=0, endvalue=10,
#'                            by='lgid', where='yearid between 2000 and 2013')
#' createPopPyramid(data=eraHist, divideBy='lgid', values=c('NL','AL'),
#'                  title="ERA Pyramid by MLB Leagues", xlab='ERA', ylab='Player Count')
#'
#' # Log ERA
#' eraLogHist = computeHistogram(asterConn, tableName='pitching', columnName='era_log', binsize=.02, startvalue=-0.42021640338318984325, 
#'                               endvalue=2.2764618041732441,
#'                               by='lgid', where='yearid between 2000 and 2013 and era > 0')
#' createPopPyramid(data=eraLogHist, divideBy='lgid', values=c('NL','AL'),
#'                  title="log(ERA) Pyramid by MLB Leagues", xlab='log(ERA)', ylab='Player Count')
#'
#' # Batting (BA)
#' battingHist = computeHistogram(asterConn, tableName='batting_enh', columnName='ba', binsize=.01, startvalue=0.01, endvalue=0.51,
#'                                by='lgid', where='yearid between 2000 and 2013')
#' createPopPyramid(data=battingHist, divideBy='lgid', values=c('NL','AL'),
#'                  title="Batting BA Pyramid by MLB Leages", xlab='BA', ylab='Player Count')
#' }
createPopPyramid <- function(data, bin = 'bin_start', count = 'bin_count', divideBy, 
                             values = NULL, fillColours=c('blue','red'), mainColour="black",
                             facet = NULL, ncol = 1, facetScales = "fixed",
                             baseSize = 12, baseFamily = "sans", 
                             title=paste("Population Pyramid Histogram"), xlab = bin, ylab = count,
                             legendPosition = "right",
                             defaultTheme = theme_bw(base_size = baseSize),
                             themeExtra = NULL) {
  if (missing(values)) {
    values = sort(unique(data[, divideBy]))[1:2]
  }
  
  data1 = data[with(data, get(divideBy)) == values[[1]],]
  data1[, count] = -data1[, count]
  data2 = data[with(data, get(divideBy)) == values[[2]],]
  
  p = ggplot(data, aes_string(x=bin, y=count, fill=divideBy)) +
    geom_histogram(data=data1, stat="identity", colour=mainColour, fill=fillColours[[1]]) +
    geom_histogram(data=data2, stat="identity", colour=mainColour, fill=fillColours[[2]]) +
    coord_flip() +
    defaultTheme +
    labs(title=title, x=xlab, y=ylab) +
    theme(legend.position = legendPosition, 
          plot.title = element_text(family = baseFamily, face = "bold", size = baseSize * 1.4, vjust = 1)) +
    themeExtra
  
  p = applyFacet(p, facet, facetScales, ncol)
  
  return(p)
}


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
    textLayer = geom_text(data=data, aes_string(x=x, y=y, label=".value.text", fill=fill),
                         stat="identity", 
                         position=position, 
                         hjust=hjust, vjust=vjust)
  }else {
    return (NULL)
  }
}

# Applies standard set of facet parameters to create \code{facet_wrap} or \code{facet_grid}
# 
# @param p \code{ggplot2} plot object
# @param facet vector of columns (1 or 2) to use for facet(s)
# @param scales facet scale option
# @param ncol number of facet columns (applies when single facet column supplied only)
# 
applyFacet <- function(p, facet=NULL, scales, ncol) {
  if (!missing(facet) & length(facet) > 0) {
    if (length(facet) == 1) {
      p = p + facet_wrap(as.formula(paste("~", facet)), ncol=ncol, scales=scales)
    }else {
      p = p + facet_grid(as.formula(paste(facet[[1]], "~", facet[[2]])), scales=scales)
    }   
  }
  
  return(p)
}


# Generate gradient palette maker
# 
# inspired by 
# http://stackoverflow.com/questions/13353213/gradient-of-n-colors-ranging-from-color-1-and-color-2
# 
# @param colors pair of colors for gradient range (min, max): default is \code{c('black','white')}
# @return function that 
# @seealso \code{\link{colorRampPalette}}
#
# @examples
# paletteMaker = colorGradientPalette(c("orange","red"))
# myPalette = paletteMaker(10)
colorGradientPalette <- function(colors=c("black", "white")) {
  colfunc = colorRampPalette(colors)
  return(colfunc)
}

# Generate discrete palette maker
# 
# @param paletteName name of palette from \code{brewer.pal.info} in \code{RColorBrewer} package
# 
# @export
# @examples
# paletteMaker = colorDiscretePalette("PuOr")
# myPalette = paletteMaker(25)
colorDiscretePalette <- function(paletteName="Set1") {
  n = brewer.pal.info[rownames(brewer.pal.info)==paletteName, 'maxcolors']
  colfunc = colorRampPalette(brewer.pal(n, paletteName))
  return(colfunc)
}

#' Creates empty theme 
#' 
#' Good to use with slopegraphs.
#' 
#' @param baseSize base font size
#' @param baseFamily base font family
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