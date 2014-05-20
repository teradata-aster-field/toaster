\name{nGram}
\alias{nGram}
\title{Tokenize (or split) text and emit n multi-grams.}
\usage{
nGram(n, delimiter = NULL, overlapping = TRUE, ignoreCase = FALSE,
  punctuation = NULL, reset = NULL, sep = " ")
}
\arguments{
  \item{n}{length, in words, of each n-gram}

  \item{delimiter}{character or string that divides one
  word from the next.  You can use a regular expression as
  the \code{delimiter} value.}

  \item{overlapping}{logical: true value allows for
  overlapping n-grams.}

  \item{ignoreCase}{logical: if FALSE, the n-gram matching
  is case sensitive and if TRUE, case is ignored during
  matching.}

  \item{punctuation}{a regular expression that specifies
  the punctuation characters parser will remove before it
  evaluates the input text.}

  \item{reset}{a regular expression listing one or more
  punctuation characters or strings, any of which the
  \code{nGram} parser will recognize as the end of a
  sentence of text. The end of each sentence resets the
  search for n-grams, meaning that \code{nGram} discards
  any partial n-grams and proceeds to the next sentence to
  search for the next n-gram. In other words, no n-gram can
  span two sentences.}

  \item{sep}{a character string to separate multiple text
  columns.}
}
\description{
Tokenize (or split) text and emit n multi-grams.
}

