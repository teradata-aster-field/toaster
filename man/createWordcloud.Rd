\name{createWordcloud}
\alias{createWordcloud}
\title{Create Word Cloud Visualization.}
\usage{
createWordcloud(words, freq, title = "Wordcloud", scale = c(8, 0.2),
  minFreq = 10, maxWords = 40, filename, format = c("png", "bmp", "jpeg",
  "tiff", "pdf"), width = 480, height = 480, units = "px",
  palette = brewer.pal(8, "Dark2"), titleFactor = 1)
}
\arguments{
  \item{words}{the words}

  \item{freq}{their frequencies}

  \item{title}{plot title}

  \item{scale}{a vector indicating the range of the size of
  the words (default c(4,.5))}

  \item{minFreq}{words with frequency below \code{minFreq}
  will not be displayed}

  \item{maxWords}{Maximum number of words to be plotted
  (least frequent terms dropped).}

  \item{filename}{file name to use where to save graphics}

  \item{format}{format of graphics device to save wordcloud
  image}

  \item{width}{the width of the output graphics device}

  \item{height}{the height of the output graphics device}

  \item{units}{the units in which \code{height} and
  \code{width} are given. Cab be \code{px} (pixels, the
  default), \code{in} (inches), \code{cm} or \code{mm}.}

  \item{palette}{color words from least to most frequent}

  \item{titleFactor}{numeric title character expansion
  factor; multiplied by \code{\link{par}("cex")} yields the
  final title character size. NULL and NA are equivalent to
  a factor of 1.}
}
\value{
nothing
}
\description{
Wrapper around \code{\link{wordcloud}} function that
optionally saves graphics to the file of one of supported
formats.
}
\details{
Uses base graphics and worldcloud package to create a word
cloud (tag cloud) visual reprsentation of for text data.
Function uses 2 vectors of equal lengths: one contains list
of words and the other has their frequencies.

Resulting graphics is saved in file in one of available
graphical formats (png, bmp, jpeg, tiff, or pdf).

Word Cloud visuals apply to any concept that satisfies
following conditions: * each data point (artifact) can be
expressed with distinct word or compact text in distinct
and self-explanatory fashion and * it assigns each artifact
scalar non-negative metric. Given these two conditions we
can use Word Clouds to visualize top, bottom or all
artifacts in single word cloud visual.
}
\seealso{
\code{\link{wordcloud}}
}

