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
  \item{data}{data set to visualize}

  \item{x}{name of a column containing x variable}

  \item{y}{name of a column containing y variable}

  \item{z}{name of a column containing bubble value}

  \item{label}{name of a column containing bubble label}

  \item{fill}{name of a column to use for fill colour}

  \item{facet}{}

  \item{ncol}{}

  \item{facetScales}{}

  \item{xlim}{}

  \item{baseSize}{}

  \item{baseFamily}{}

  \item{shape}{bubble shape (default is 21)}

  \item{shapeSizeRange}{bubble size range}

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

