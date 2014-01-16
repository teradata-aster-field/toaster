\name{createUS48Map}
\alias{createUS48Map}
\title{Creates map with 48 contiguous US states.}
\usage{
  createUS48Map()
}
\arguments{
  \item{states}{vector of location strings to include in
  the map. Usually are names of states, cities, counties or
  mix. Each location will be geocoded against Google API
  and average of resulting longitudes and lattitudes used.
  Use \code{zoom} and \code{scale} to fit them all on
  resulting map. If you dataset has been geocoded you may
  want not to use it.}
}
\description{
  Creates map with 48 contiguous US states.
}

