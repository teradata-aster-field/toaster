% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{createBoxplot}
\alias{createBoxplot}
\title{Create box plot.}
\usage{
createBoxplot(data, x = NULL, fill = x, value = "value", useIQR = FALSE,
  facet = NULL, ncol = 1, facetScales = "fixed", paletteValues = NULL,
  palette = "Set1", title = paste("Boxplots", ifelse(is.null(x), NULL,
  paste("by", x))), subtitle = NULL, xlab = x, ylab = NULL,
  legendPosition = "right", fillGuide = "legend", coordFlip = FALSE,
  baseSize = 12, baseFamily = "sans", defaultTheme = theme_tufte(base_size
  = baseSize, base_family = baseFamily), themeExtra = NULL)
}
\arguments{
\item{data}{quartiles precomputed with \code{\link{computePercentiles}}}

\item{x}{column name of primary variance. Multiple boxplots are placed
along the x-axis. Each value of \code{x} must have corresponding
percentiles calculated.}

\item{fill}{name of a column with values to colour box plots}

\item{value}{column name with percentile value. Usually default \code{'value'}
with exception of temporal percentiles that should use \code{'epoch'} value.}

\item{useIQR}{logical indicates use of IQR interval to compute cutoff lower 
and upper bounds: \code{[Q1 - 1.5 * IQR, Q3 + 1.5 * IQR], IQR = Q3 - Q1}, 
if FALSE then use maximum and minimum bounds (all values).}

\item{facet}{vector of 1 or 2 column names to split up data to plot the 
subsets as facets. If single name then subset plots are placed next to 
each other, wrapping with \code{ncol} number of columns (uses \code{\link{facet_wrap}}). 
When two names then subset plots vary on both horizontal and vertical 
directions (grid) based on the column values (uses \code{\link{facet_grid}}).}

\item{ncol}{number of facet columns (applies when single facet column supplied only 
- see parameter \code{facet}).}

\item{facetScales}{Are scales shared across all subset plots (facets): 
"fixed" - all are the same, "free_x" - vary across rows (x axis), 
"free_y" - vary across columns (Y axis, default), "free" - both rows and 
columns (see in \code{facet_wrap} parameter \code{scales} )}

\item{paletteValues}{actual palette colours for use with \code{scale_fill_manual} (if specified then parameter
\code{palette} is ignored)}

\item{palette}{Brewer palette name - see \code{display.brewer.all} in \code{RColorBrewer} package for names}

\item{title}{plot title.}

\item{subtitle}{plot subtitle.}

\item{xlab}{a label for the x axis, defaults to a description of x.}

\item{ylab}{a label for the y axis, defaults to a description of y.}

\item{legendPosition}{the position of legends. ("left", "right", "bottom", "top", or two-element numeric 
vector). "none" is no legend.}

\item{fillGuide}{Name of guide object, or object itself for the \code{fill} (when present). Typically \code{"legend"} name
or object \code{\link[ggplot2]{guide_legend}}.}

\item{coordFlip}{logical flipped cartesian coordinates so that horizontal becomes vertical, and vertical horizontal (see 
\link{coord_flip}).}

\item{baseSize}{\code{\link{theme}} base font size}

\item{baseFamily}{\code{\link{theme}} base font family}

\item{defaultTheme}{plot theme settings with default value \code{\link[ggthemes]{theme_tufte}}. More themes
are available here: \code{\link[ggplot2]{ggtheme}} (by \href{http://ggplot2.org/}{ggplot2}) 
and \code{\link[ggthemes]{ggthemes}}.}

\item{themeExtra}{any additional \code{\link[ggplot2]{theme}} settings that override default theme.}
}
\value{
ggplot object
}
\description{
Create box plot visualization using quartiles calculated with \code{\link{computePercentiles}}.
The simplest case without x value displays single boxplot from the
single set of percentiles. To plot multiple box plots and multiple or single 
box plots with facets use parameters \code{x} and/or \code{facet}.
}
\details{
Multiple box plots: \code{x} is a name of variable where each value 
corresponds to a set of percentiles. The boxplots will be placed along 
the  x-axis. Simply use \code{\link{computePercentiles}} with parameter
\code{by="name to be passed in x variable"}.

Facets: \code{facet} vector contains one or two names of vairables where
each combination of values corresponds to a set of percentiles. The
boxplot(s) will be placed inside separate sections of the plot (facets).
Both single boxplot (without variable \code{x} and with one) are 
supported.

Usually, with multiple percentile sets varying along single value 
use parameter \code{x} and add facets on top. The exception is
when scale of percentile values differs between each
boxplot. Then omit parameter \code{x} and use
\code{facet} with \code{facetScales='free_y'}.
}
\examples{
if(interactive()){
# initialize connection to Lahman baseball database in Aster 
conn = odbcDriverConnect(connection="driver={Aster ODBC Driver};
                         server=<dbhost>;port=2406;database=<dbname>;uid=<user>;pwd=<pw>")

# boxplot of pitching ipouts for AL in 2000s
ipop = computePercentiles(conn, "pitching", columns="ipouts")
createBoxplot(ipop)
                          
# boxplots by the league of pitching ipouts
ipopLg = computePercentiles(conn, "pitching", columns="ipouts", by="lgid")
createBoxplot(ipopLg, x="lgid")

# boxplots by the league with facet yearid of pitching ipouts in 2010s
ipopLgYear = computePercentiles(conn, "pitching", columns="ipouts", by=c("lgid", "yearid"),
                                where = "yearid >= 2010")
createBoxplot(ipopLgYear, x="lgid", facet="yearid", ncol=3)

# boxplot with facets only
bapLgDec = computePercentiles(conn, "pitching_enh", columns="era", by=c("lgid", "decadeid"),
                              where = "lgid in ('AL','NL')")
createBoxplot(bapLgDec, facet=c("lgid", "decadeid"))
}
}
\seealso{
\code{\link{computePercentiles}} for computing boxplot quartiles
}

