\name{createHistogram}
\alias{createHistogram}
\title{Create plot with Histogram visualization}
\usage{
  createHistogram(data, x = "bin_start", y = "bin_count",
    fill = NULL, position = "dodge", facet = NULL,
    ncol = 1, facetScales = "free_y", baseSize = 12,
    baseFamily = "sans", xlim = NULL, breaks = NULL,
    text = FALSE, percent = FALSE, digits = 0,
    textVJust = -2, mainColour = "black",
    fillColour = "grey", scaleGradient = NULL,
    paletteValues = NULL, palette = "Set1",
    trendLines = FALSE,
    title = paste("Histgoram by", fill), xlab = x,
    ylab = y, legendPosition = "right", coordFlip = FALSE,
    defaultTheme = theme_bw(base_size = baseSize),
    themeExtra = NULL)
}
\arguments{
  \item{position}{histogram position parameter to use for
  overlapping bars: stack, dodge (defult), fill, identity}

  \item{mainColour}{Perimeter color of histogram bars}

  \item{fillColour}{Fill color of histogram bars (applies
  only when \code{fill} is NULL)}

  \item{scaleGradient}{control \code{ggplot2} scale fill
  gradient manually, e.g use \code{scale_colour_gradient}
  (if specified then parameter \code{palette} is ignored)}

  \item{paletteValues}{actual palette colours for use with
  \code{scale_fill_manual} (if specified then parameter
  \code{palette} is ignored)}

  \item{palette}{Brewer palette name - see
  \code{display.brewer.all} in \code{RColorBrewer} package
  for names}

  \item{facet}{Divide plot into facets for specificed
  parameter (defualt is NULL - no facets). If facet is a
  single value then facet wrap applied, otherwise facet
  grid using first and second values and ignoring the
  rest.}

  \item{ncol}{Number of columns in facet wrap}

  \item{facetScales}{Are scales shared across all facets:
  "fixed" - all are the same, "free_x" - vary across rows
  (x axis), "free_y" - vary across columns (Y axis)
  (default), "free" - both rows and columns (see in
  \code{facet_wrap} parameter \code{scales} )}

  \item{text}{if TRUE then display values above bars
  (default: FALSE) (this feature is in development)}

  \item{percent}{format text as percent}

  \item{digits}{number of digits to use in text}

  \item{textVJust}{vertical justificaiton of text labels
  (relative to the top of bar)}

  \item{legendPosition}{the position of legends. ("left",
  "right", "bottom", "top", or two-element numeric vector).
  "none" is no legend.}

  \item{coordFlip}{logical flipped cartesian coordinates so
  that horizontal becomes vertical, and vertical,
  horizontal (see \link{coord_flip}).}

  \item{defaultTheme}{plot theme to use, default is
  \code{theme_bw}}

  \item{themeExtra}{any additional \code{ggplot2} theme
  attributes to add}
}
\description{
  Create plot with Histogram visualization
}
\examples{
\donttest{
# compute and plot histograms of AL teams pitching stats
bc = computeBarchart(conn, "pitching_enh", "teamid",
aggregates=c("AVG(era) era", "AVG(whip) whip", "AVG(ktobb) ktobb"),
where="yearid >= 1990 and lgid='AL'", by="decadeid", withMelt=TRUE)

createHistogram(bc, "teamid", "value", fill="teamid", facet=c("variable", "decadeid"), legendPosition="bottom",
                title = "AL Teams Pitching Stats by decades (1990-2012)",
                themeExtra = guides(fill=guide_legend(nrow=2)))
}
}

