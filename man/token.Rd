\name{token}
\alias{token}
\title{Tokenize (or split) text and emit n-word combinations from a document.}
\usage{
token(n, tokenSep = "+", ignoreCase = FALSE,
  delimiter = "[ \\\\t\\\\b\\\\f\\\\r]+", punctuation = NULL,
  stemming = FALSE, stopWordsFile = NULL, sep = " ")
}
\arguments{
  \item{n}{number of words}
}
\description{
When n=1 simply tokenize text and emit words with counts.
When n>1 tokenized words are combined into permutations of
length n within each document.
}

