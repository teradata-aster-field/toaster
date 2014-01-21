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
    paletteValues = NULL, palette = "Set1", trend = FALSE,
    trendLinetype = "solid", trendLinesize = 1,
    trendLinecolour = "black",
    title = paste("Histgoram by", fill), xlab = x,
    ylab = y, legendPosition = "right", coordFlip = FALSE,
    defaultTheme = theme_bw(base_size = baseSize),
    themeExtra = NULL)
}
\arguments{
  \item{data}{data frame contains computed histogram}

  \item{x}{name of a column containing bin labels or
  interval values}

  \item{y}{name of a column containing bin values or counts
  (bin size)}

  \item{fill}{name of a column with values to colour bars}

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

  \item{baseSize}{base font size}

  \item{baseFamily}{base font family}

  \item{xlim}{a character vector specifying the data range
  for the x scale and the default order of their display in
  the x axis.}

  \item{breaks}{a character vector giving the breaks as
  they should appear on the x axis.}

  \item{text}{if TRUE then display values above bars
  (default: FALSE) (this feature is in development)}

  \item{percent}{format text as percent}

  \item{digits}{number of digits to use in text}

  \item{textVJust}{vertical justificaiton of text labels
  (relative to the top of bar).}

  \item{trend}{logical indicates if trend line is shown.}

  \item{trendLinetype}{trend line type}

  \item{trendLinesize}{size of trend line}

  \item{trendLinecolour}{color of trend line}

  \item{title}{plot title}

  \item{xlab}{a label for the x axis, defaults to a
  description of x.}

  \item{ylab}{a label for the y axis, defaults to a
  description of y.}

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
# AL teams pitching stats by decade
bc = computeBarchart(conn, "pitching_enh", "teamid",
aggregates=c("AVG(era) era", "AVG(whip) whip", "AVG(ktobb) ktobb"),
where="yearid >= 1990 and lgid='AL'", by="decadeid", withMelt=TRUE)

createHistogram(bc, "teamid", "value", fill="teamid",
                facet=c("variable", "decadeid"),
                legendPosition="bottom",
                title = "AL Teams Pitching Stats by decades (1990-2012)",
                themeExtra = guides(fill=guide_legend(nrow=2)))

# AL Teams Average Win-Loss Difference by Decade
franchwl = computeBarchart(conn, "teams_enh", "franchid",
                           aggregates=c("AVG(w) w", "AVG(l) l", "AVG(w-l) wl"),
                           by="decadeid",
                           where="yearid >=1960 and lgid = 'AL'")

createHistogram(franchwl, "decadeid", "wl", fill="franchid",
                facet="franchid", ncol=5, facetScales="fixed",
                legendPosition="none",
                trend=TRUE,
                title="Average W-L difference by decade per team (AL)",
                ylab="Average W-L")

}
}

