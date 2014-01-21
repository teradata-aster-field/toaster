\name{createBubblechart}
\alias{createBubblechart}
\title{Create plot with Bubble Chart visualization}
\usage{
  createBubblechart(data, x, y, z, label = z, fill = NULL,
    facet = NULL, ncol = 1, facetScales = "fixed",
    xlim = NULL, baseSize = 12, baseFamily = "sans",
    shape = 21, shapeSizeRange = c(1, 10),
    title = paste("Bubble Chart by", fill), xlab = x,
    ylab = y, textSize = 4, textColour = "black",
    textVJust = 1, legendPosition = "right",
    defaultTheme = theme_bw(base_size = baseSize),
    themeExtra = NULL)
}
\arguments{
  \item{data}{data frame contains data computed for
  bubblechart}

  \item{x}{name of a column containing x variable values}

  \item{y}{name of a column containing y variable values}

  \item{z}{name of a column containing bubble size value}

  \item{label}{name of a column containing bubble label}

  \item{fill}{name of a column with values to use for
  bubble colours}

  \item{facet}{name of a column to divide plot into facets
  for specificed parameter (defualt is NULL - no facets).
  If facet is single value then facet wrap applied (see
  \code{\link{facet_wrap}}), otherwise facet grid (see
  \code{\link{facet_grid}} with 1st 2 values of the
  vector.}

  \item{ncol}{number of facet columns (applies when single
  facet column supplied only - see parameter
  \code{facet}).}

  \item{facetScales}{Are scales shared across all facets:
  "fixed" - all are the same, "free_x" - vary across rows
  (x axis), "free_y" - vary across columns (Y axis)
  (default), "free" - both rows and columns (see in
  \code{facet_wrap} parameter \code{scales} )}

  \item{xlim}{a vector specifying the data range for the x
  scale and the default order of their display in the x
  axis.}

  \item{baseSize}{base font size}

  \item{baseFamily}{base font family}

  \item{shape}{bubble shape (default is 21)}

  \item{shapeSizeRange}{bubble size range}

  \item{title}{plot title}

  \item{xlab}{a label for the x axis, defaults to a
  description of x.}

  \item{ylab}{a label for the y axis, defaults to a
  description of y.}

  \item{textSize}{size of text labels}

  \item{textColour}{color of text labels}

  \item{textVJust}{vertical justificaiton of text labels
  (relative to the center of bubble)}

  \item{legendPosition}{the position of legends. ("left",
  "right", "bottom", "top", or two-element numeric vector).
  "none" is no legend.}

  \item{defaultTheme}{plot theme to use, default is
  \code{theme_bw}}

  \item{themeExtra}{any additional \code{ggplot2} theme
  attributes to add}
}
\description{
  Create plot with Bubble Chart visualization
}

