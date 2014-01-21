\name{createHeatmap}
\alias{createHeatmap}
\title{Create plot with Heatmap visualization}
\usage{
  createHeatmap(data, x, y, fill, facet = NULL, ncol = 1,
    baseSize = 12, baseFamily = "sans",
    thresholdValue = NULL, thresholdName = fill,
    text = FALSE, textFill = fill, percent = FALSE,
    digits = ifelse(percent, 2, 4),
    divergingColourGradient = FALSE,
    lowGradient = ifelse(divergingColourGradient, muted("red"), "#56B1F7"),
    midGradient = "white",
    highGradient = ifelse(divergingColourGradient, muted("blue"), "#132B43"),
    title = paste("Heatmap by", fill), xlab = x, ylab = y,
    legendPosition = "right",
    defaultTheme = theme_bw(base_size = baseSize),
    themeExtra = NULL)
}
\arguments{
  \item{data}{data frame contains data computed for
  heatmap}

  \item{x}{name of a column containing x variable values}

  \item{y}{name of a column containing y variable values}

  \item{fill}{name of a column with values to map to
  heatmap gradient colors (\code{lowGradient},
  \code{highGradient}, and optionally \code{midGradient}).}

  \item{facet}{name of a column to divide plot into facets
  for specificed parameter (defualt is NULL - no facets).
  If facet is single value then facet wrap applied (see
  \code{\link{facet_wrap}}), otherwise facet grid (see
  \code{\link{facet_grid}} with 1st 2 values of the
  vector.}

  \item{ncol}{number of facet columns (applies when single
  facet column supplied only - see parameter
  \code{facet}).}

  \item{baseSize}{base font size}

  \item{baseFamily}{base font family}

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

  \item{title}{plot title}

  \item{xlab}{a label for the x axis, defaults to a
  description of x.}

  \item{ylab}{a label for the y axis, defaults to a
  description of y.}

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

