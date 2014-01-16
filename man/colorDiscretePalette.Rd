\name{colorDiscretePalette}
\alias{colorDiscretePalette}
\title{Generate discrete palette maker}
\usage{
  colorDiscretePalette(paletteName = "Set1")
}
\arguments{
  \item{paletteName}{name of palette from
  \code{brewer.pal.info} in \code{RColorBrewer} package}
}
\description{
  Generate discrete palette maker
}
\examples{
paletteMaker = colorDiscretePalette("PuOr")
myPalette = paletteMaker(25)
}

