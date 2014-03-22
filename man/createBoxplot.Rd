\name{createBoxplot}
\alias{createBoxplot}
\title{Create box plot}
\usage{
  createBoxplot(data, x = NULL, facet = NULL, ncol = 1,
    facetScales = "fixed",
    title = paste("Bubble Chart by", fill), xlab = x,
    ylab = NULL, legendPosition = "right",
    coordFlip = FALSE, baseSize = 12, baseFamily = "sans",
    defaultTheme = theme_bw(base_size = baseSize, base_family = baseFamily),
    themeExtra = NULL)
}
\arguments{
  \item{data}{quartiles precomputed with
  \code{\link{computePercentiles}}}

  \item{x}{variable name of primary variance by x-axis}

  \item{facet}{vector of 1 or 2 column names to split up
  data to plot the subsets as facets. If single name then
  subset plots are placed next to each other, wrapping with
  \code{ncol} number of columns (uses
  \code{\link{facet_wrap}}).  When two names then subset
  plots vary on both horizontal and vertical directions
  (grid) based on the column values (uses
  \code{\link{facet_grid}}).}

  \item{ncol}{number of facet columns (applies when single
  facet column supplied only - see parameter
  \code{facet}).}

  \item{facetScales}{Are scales shared across all subset
  plots (facets): "fixed" - all are the same, "free_x" -
  vary across rows (x axis), "free_y" - vary across columns
  (Y axis, default), "free" - both rows and columns (see in
  \code{facet_wrap} parameter \code{scales} )}

  \item{title}{plot title.}

  \item{xlab}{a label for the x axis, defaults to a
  description of x.}

  \item{ylab}{a label for the y axis, defaults to a
  description of y.}

  \item{baseSize}{\code{\link{theme}} base font size}

  \item{baseFamily}{\code{\link{theme}} base font family}

  \item{legendPosition}{the position of legends. ("left",
  "right", "bottom", "top", or two-element numeric vector).
  "none" is no legend.}

  \item{coordFlip}{logical flipped cartesian coordinates so
  that horizontal becomes vertical, and vertical horizontal
  (see \link{coord_flip}).}

  \item{defaultTheme}{plot theme to use:
  \code{\link[ggplot2]{theme_bw}} (default),
  \code{\link[ggplot2]{theme_grey}},
  \code{\link[ggplot2]{theme_classic}} or custom.}

  \item{themeExtra}{any additional
  \code{\link[ggplot2]{theme}} settings that override
  default theme.}
}
\description{
  Create box plot visualization using quartiles calculated
  with \code{\link{computePercentiles}}.
}
\examples{
\donttest{
# boxplot of pitching ipouts for AL in 2000s
ipop = computePercentiles(conn, "pitching", "ipouts",
                          where = "lgid = 'AL' and yearid >= 2000")
createBoxplot(ipop)

# boxplots by the league of pitching ipouts
ipopLg = computePercentiles(conn, "pitching", "ipouts", by="lgid")
createBoxplot(ipopLg, x="lgid")

# boxplots by the league with facet yearid of pitching ipouts in 2010s
ipopLgYear = computePercentiles(conn, "pitching", "ipouts", by=c("lgid", "yearid"),
                                where = "yearid >= 2010")
createBoxplot(ipopLgYear, x="lgid", facet="yearid")

}
}
\seealso{
  \code{\link{computePercentiles}} for computing boxplot
  quartiles
}

