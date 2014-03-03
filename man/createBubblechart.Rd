\name{createBubblechart}
\alias{createBubblechart}
\title{Create Bubble Chart plot}
\usage{
  createBubblechart(data, x, y, z, label = z, fill = NULL,
    facet = NULL, ncol = 1, facetScales = "fixed",
    xlim = NULL, baseSize = 12, baseFamily = "sans",
    shape = 21, shapeColour = "black", scaleSize = TRUE,
    shapeSizeRange = c(3, 10), shapeMaxSize = 100,
    paletteValues = NULL, palette = "Set1",
    title = paste("Bubble Chart by", fill), xlab = x,
    ylab = y, labelSize = 5, labelFamily = "",
    labelFontface = "plain", labelColour = "black",
    labelVJust = 0.5, labelHJust = 0.5, labelAlpha = 1,
    labelAngle = 0, legendPosition = "right",
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

  \item{shape}{bubble shape}

  \item{shapeColour}{colour of shapes}

  \item{scaleSize}{logical if TRUE then scale the size of
  shape to be proportional to the value, if FALSE then
  scale the area.}

  \item{shapeSizeRange}{bubble size range (applies only
  when \code{scaleSize = TRUE})}

  \item{shapeMaxSize}{size of largest shape (applies only
  when \code{scaleSize = FALSE})}

  \item{paletteValues}{actual palette colours for use with
  \code{scale_fill_manual} (if specified then parameter
  \code{palette} is ignored)}

  \item{palette}{Brewer palette name - see
  \code{display.brewer.all} in \code{RColorBrewer} package
  for names}

  \item{title}{plot title}

  \item{xlab}{a label for the x axis, defaults to a
  description of x.}

  \item{ylab}{a label for the y axis, defaults to a
  description of y.}

  \item{labelSize}{size of labels}

  \item{labelFamily}{label font name or family name}

  \item{labelFontface}{label font face
  (\code{c("plain","bold","italic","bold.italic")})}

  \item{labelColour}{color of labels}

  \item{labelVJust}{position of the anchor (0=bottom edge,
  1=top edge), can go below 0 or above 1}

  \item{labelHJust}{position of the label anchor (0=left
  edge, 1=right edge), can go below 0 or above 1}

  \item{labelAlpha}{the transparency of the text label}

  \item{labelAngle}{the angle at which to draw the text
  label}

  \item{legendPosition}{the position of legends. ("left",
  "right", "bottom", "top", or two-element numeric vector).
  "none" is no legend.}

  \item{defaultTheme}{plot theme to use, default is
  \code{theme_bw}}

  \item{themeExtra}{any additional \code{ggplot2} theme
  attributes to add}
}
\description{
  Create Bubble Chart plot
}

