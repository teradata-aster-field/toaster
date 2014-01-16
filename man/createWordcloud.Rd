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

  \item{filename}{file name to use where to save graphics}

  \item{format}{format of graphics device to use}
}
\description{
  Uses base graphics.
}

