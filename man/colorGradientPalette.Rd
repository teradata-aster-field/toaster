\name{colorGradientPalette}
\alias{colorGradientPalette}
\title{Generate gradient palette maker}
\usage{
  colorGradientPalette(colors = c("black", "white"))
}
\arguments{
  \item{colors}{pair of colors for gradient range (min,
  max): default is \code{c('black','white')}}
}
\description{
  inspired by
  http://stackoverflow.com/questions/13353213/gradient-of-n-colors-ranging-from-color-1-and-color-2
}
\examples{
paletteMaker = colorGradientPalette(c("orange","red"))
myPalette = paletteMaker(10)
}

