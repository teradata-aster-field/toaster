\name{createWordcloud}
\alias{createWordcloud}
\title{Create plot with Word Cloud Visualization}
\usage{
  createWordcloud(words, freq, title = "Wordcloud",
    scale = c(8, 0.2), minFreq = 10, maxWords = 40,
    filename,
    format = c("png", "bmp", "jpeg", "tiff", "pdf"),
    width = 480, height = 480, units = "px",
    palette = brewer.pal(8, "Dark2"), textCex = 1)
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

  \item{textCex}{title text size}
}
\description{
  Uses base graphics.
}

