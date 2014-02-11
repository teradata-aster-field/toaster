\name{createPopPyramid}
\alias{createPopPyramid}
\title{Population Pyramid Visual}
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
  \item{data}{data frame contains 2 histograms for the same
  bins. Bins are divided into 2 sets with parameter
  \code{divideBy}.}

  \item{bin}{name of a column containing bin labels or
  interval values}

  \item{count}{name of a column containing bin values or
  counts (bin size)}

  \item{divideBy}{name of the column to divide data into
  two histograms}

  \item{values}{two-valued vector containing values in
  \code{divideBy} (optional). If missing then it uses 1st 2
  values from column \code{divideBy} (sorted with default
  order).}

  \item{fillColours}{2-value vector with colours for left
  and right histograms.}

  \item{mainColour}{histogram bar colour.}

  \item{baseSize}{base font size}

  \item{baseFamily}{base font family}

  \item{title}{plot title.}

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
  Create population pyramid type of histogram.
}
\details{
  Population pyramid consists of 2 parallel histograms
  defined on the same bin set and placed on the same
  vertical axis with one set of bars on the left and the
  other on the right.
}

