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
  \item{defaultTheme}{plot theme to use, default is
  \code{theme_bw}}

  \item{themeExtra}{any additional \code{ggplot2} theme
  attributes to add}
}
\description{
  Create plot with Slope Graph visualization
}

