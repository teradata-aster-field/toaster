\name{createHeatmap}
\alias{createHeatmap}
\title{Create plot with Heatmap visualization}
\usage{
  createHeatmap(data, x, y, fill, facet = NULL, ncol = 1,
    baseSize = 12, baseFamily = "sans",
    thresholdValue = NULL, thresholdName = fill,
    text = FALSE, textFill = fill, percent = FALSE,
    digits = ifelse(percent, 2, 4),
    title = paste("Heatmap by", fill), xlab = NULL,
    ylab = NULL, divergingColourGradient = FALSE,
    lowGradient = ifelse(divergingColourGradient, muted("red"), "#132B43"),
    midGradient = "white",
    highGradient = ifelse(divergingColourGradient, muted("blue"), "#56B1F7"),
    legendPosition = "right",
    defaultTheme = theme_bw(base_size = baseSize),
    themeExtra = NULL)
}
\arguments{
  \item{thresholdValue}{threshold to use to display data in
  heatmap (if NULL then do not use threshold)}

  \item{thresholdName}{name of data attribute from
  \code{data} to use (by defult use \code{fill})}

  \item{text}{if TRUE then display values in heatmap table
  (default: FALSE)}

  \item{textFill}{text to display (applies only when
  \code{text} is TRUE), by defaul use \code{fill} values}

  \item{percent}{format text as percent}

  \item{digits}{number of digits to use in text}

  \item{lowGradient}{colour for low end of gradient.}

  \item{midGradient}{colour for mid point.}

  \item{highGradient}{colour for high end of gradient.}

  \item{divergingColourGradient}{logical diverging colour
  gradient places emphasize on both low and high leaving
  middle neutral. Use when both end grandient colours
  represent critical values such as negative and positive
  extremes (e.g. temprature, outliers, etc.)}

  \item{legendPosition}{the position of legends. ("left",
  "right", "bottom", "top", or two-element numeric vector).
  "none" is no legend.}

  \item{defaultTheme}{plot theme to use, default is
  \code{theme_bw}}

  \item{themeExtra}{any additional \code{ggplot2} theme
  attributes to add}
}
\description{
  Create plot with Heatmap visualization
}

