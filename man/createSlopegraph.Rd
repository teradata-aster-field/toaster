\name{createSlopegraph}
\alias{createSlopegraph}
\title{Create plot with Slope Graph visualization}
\usage{
  createSlopegraph(data, id, rankFrom, rankTo,
    reverse = TRUE, na.rm = FALSE, scaleFactor = 1,
    fromLabel = rankFrom, toLabel = rankTo,
    title = paste("Slopegraph by", rankTo), baseSize = 12,
    baseFamily = "sans", classLabels = c(rankFrom, rankTo),
    classTextSize = 12, colour = "#999999",
    upColour = "#D55E00", downColour = "#009E73",
    highlights = integer(0), lineSize = 0.15,
    textSize = 3.75, panelGridColour = "black",
    panelGridSize = 0.1,
    defaultTheme = theme_bw(base_size = baseSize),
    themeExtra = NULL)
}
\arguments{
  \item{data}{data frame contains data computed for
  slopegraph}

  \item{id}{name of column identifying each graph element
  having from (before) and to (after) pair of values}

  \item{rankFrom}{name of column with from (before) value}

  \item{rankTo}{name of column with to (after) value}

  \item{reverse}{logical reverse values if TRUE (smaller is
  better)}

  \item{na.rm}{logical value indicating whether NA values
  should be stripped before the visualization proceeds.}

  \item{scaleFactor}{scale factor applied to all values (-1
  can be used instead of \code{reverse} TRUE).}

  \item{fromLabel}{label for left values (from or before)}

  \item{toLabel}{label for right values (to or after)}

  \item{classLabels}{pair of labels for to and from columns
  (or classes)}

  \item{classTextSize}{size of text for class labels}

  \item{colour}{default colour}

  \item{upColour}{colour of up slope}

  \item{downColour}{colour of down slope}

  \item{highlights}{vector with indexes of highlighted
  points}

  \item{lineSize}{size of slope lines}

  \item{textSize}{size of text}

  \item{panelGridColour}{background panel grid colour}

  \item{panelGridSize}{background panel grid size}

  \item{title}{plot title}

  \item{baseSize}{base font size}

  \item{baseFamily}{base font family}

  \item{defaultTheme}{plot theme to use, default is
  \code{theme_bw}}

  \item{themeExtra}{any additional \code{ggplot2} theme
  attributes to add}
}
\description{
  Create plot with Slope Graph visualization
}

