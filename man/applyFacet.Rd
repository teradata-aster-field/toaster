\name{applyFacet}
\alias{applyFacet}
\title{Applies standard set of facet parameters to create \code{facet_wrap} or \code{facet_grid}}
\usage{
  applyFacet(p, facet = NULL, scales, ncol)
}
\arguments{
  \item{p}{\code{ggplot2} plot object}

  \item{facet}{vector of columns (1 or 2) to use for
  facet(s)}

  \item{scales}{facet scale option}

  \item{ncol}{number of facet columns (applies when single
  facet column supplied only)}
}
\description{
  Applies standard set of facet parameters to create
  \code{facet_wrap} or \code{facet_grid}
}

