\name{createPopPyramid}
\alias{createPopPyramid}
\title{Create population pyramid type of histogram.}
\usage{
  createPopPyramid(data, bin = "bin_start",
    count = "bin_count", divideBy, values = NULL,
    fillColours = c("blue", "red"), mainColour = "black",
    baseSize = 12, baseFamily = "sans",
    title = paste("Population Pyramid Histogram"),
    xlab = bin, ylab = count, legendPosition = "right",
    defaultTheme = theme_bw(base_size = baseSize),
    themeExtra = NULL)
}
\arguments{
  \item{data}{data frame contains computed histogram}

  \item{bin}{histogram bin column name}

  \item{count}{histogram bin count column name}

  \item{divideBy}{column to divide histogram into two
  halves}

  \item{values}{two-valued vector containing values in
  \code{divideBy} (optional)}

  \item{legendPosition}{the position of legends. ("left",
  "right", "bottom", "top", or two-element numeric vector).
  "none" is no legend.}

  \item{defaultTheme}{plot theme to use, default is
  \code{theme_bw}}

  \item{themeExtra}{any additional \code{ggplot2} theme
  attributes to add}
}
\description{
  Create population pyramid type of histogram.
}

