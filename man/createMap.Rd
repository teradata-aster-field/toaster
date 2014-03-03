\name{createMap}
\alias{createMap}
\title{Locate and grab a map, geocode and place data artifacts on it, plot them.}
\usage{
  createMap(data, maptype = "terrain",
    mapColor = c("color", "bw"),
    source = c("google", "osm", "stamen", "cloudmade"),
    location = NULL, locator = "center",
    boxBorderMargin = 10, zoom = NULL, locationName = NULL,
    lonName = "LONGITUDE", latName = "LATITUDE",
    metricName = NULL, labelName = NULL,
    scaleRange = c(1, 6), shapeColour = "red",
    textColour = "black", textFamily = "mono",
    textFace = "plain", textSize = 4, facet = NULL,
    ncol = 1, facetScales = "fixed",
    geocodeFun = memoise(geocode), getmapFun = get_map,
    urlonly = FALSE, api_key = NULL, baseSize = 12,
    baseFamily = "sans", title = NULL,
    legendPosition = "right",
    defaultTheme = theme_bw(base_size = baseSize),
    themeExtra = NULL)
}
\arguments{
  \item{data}{data frame with artifacts and their locations
  and metric(s) to be placed on the map. If location name
  is provided (with \code{locationName}) then it is used to
  gecode artifacts first. If not location then longitude
  and latitude must be provided. It is caller's
  responsibility adjust locations with value of \code{zoom}
  parameter to fit artifacts on the map.}

  \item{maptype}{map theme as defined in
  \code{\link{get_map}}. options available are 'terrain',
  'satellite', 'roadmap', and 'hybrid'}

  \item{mapColor}{color (\code{'color'}) or black-and-white
  (\code{'bw'})}

  \item{source}{Google Maps ('google'), OpenStreetMap
  ('osm'), Stamen Maps ('stamen'), or CloudMade maps
  ('cloudmade')}

  \item{location}{location of the map: longitude/latitude
  pair (in that order), or left/bottom/right/top bounding
  box: 'center' uses 2 value vector for the center of the
  map, while 'box' uses 4 value vector as
  left/bottom/right/top.  If missing then function will use
  parameter \code{locator} and \code{data} to determine map
  location.}

  \item{locator}{in absence of \code{location} specifies
  how to use data to determine map location: when 'center'
  then function averages out data point longitude and
  latitude values to get approximate cneter for the map;
  when 'box' it will use min/max of longitude and latitude
  values to determine bounding box: left/bottom/right/top.
  If parameter \code{locationName} is specified then
  function will geocode values from this column first.  If
  paramter \code{locationName} is missing then it assumes
  that data is already geocoded and stored in the columns
  with the names \code{lonName} and \code{latName}.}

  \item{boxBorderMargin}{margin size in percent of box
  sizes to increase box when computed from data locations.}

  \item{zoom}{map zoom as defined in \code{\link{get_map}}:
  an integer from 3 (continent) to 21 (building), default
  value 10 (city). Properly setting \code{zoom} for each
  map is responsibility of a caller. Zoom is optional when
  using bounding box location specification.}

  \item{locationName}{name of the column with strings to be
  geocoded to determine longitude and latitude for each
  data point. If this value is specified then parameters
  \code{lonName} and \code{latName} are ignored.}

  \item{lonName}{name of the column with longitude value.
  This value (in combination with value from column
  \code{latName}) is used to place each data point on the
  map. This parameter is ignored if \code{locationName} is
  defined.}

  \item{latName}{name of the column with latitude value.
  This value (in combination with value from column
  \code{lonName}) is used to place each data point on the
  map. This parameter is ignored if \code{locationName} is
  defined.}

  \item{metricName}{name of the column to use for the
  artifact metric when displaying data.}

  \item{scaleRange}{a numeric vector of lenght 2 that
  specifies the minimum and maximum size of the plotting
  symbol after transformation (see parameter \code{range}
  of \code{\link{scale_size}})}

  \item{labelName}{name of the column to use for the
  artifact label text when displaying data.}

  \item{shapeColour}{color of of artifact placed on map.}

  \item{textColour}{color of artifact labels on map.}

  \item{textFamily}{font family (when available) to use for
  artfiact labels.}

  \item{textFace}{font style to apply to artifact labels:
  'plain' (default), 'bold', 'italic', or 'bold.italic'.}

  \item{textSize}{font size of artifact labels.}

  \item{facet}{name of a column to divide plot into facets
  for specificed parameter (defualt is NULL - no facets).
  If facet is single value then facet wrap applied (see
  \code{\link{facet_wrap}}), otherwise facet grid (see
  \code{\link{facet_grid}} with 1st 2 values of the
  vector.}

  \item{ncol}{number of facet columns (applies when single
  facet column supplied only - see parameter
  \code{facet}).}

  \item{facetScales}{Are scales shared across all facets:
  "fixed" - all are the same, "free_x" - vary across rows
  (x axis), "free_y" - vary across columns (Y axis)
  (default), "free" - both rows and columns (see in
  \code{facet_wrap} parameter \code{scales} )}

  \item{geocodeFun}{geocode function. Default is
  \code{\link{geocode}} but due to Google API restrictions
  use memoised version, e.g. \code{memoise(geocode)},
  instead (see package \pkg{memoise}).}

  \item{getmapFun}{get map function. Defayult is
  \code{\link{get_map}} but due to map APIs restrictions
  use memoised version, e.g. \code{memose(get_map)},
  instead (see package \pkg{memoise})}

  \item{urlonly}{return url only}

  \item{api_key}{an api key for cloudmade maps}

  \item{baseSize}{base font size}

  \item{baseFamily}{base font family}

  \item{title}{plot title}

  \item{legendPosition}{the position of legends. ("left",
  "right", "bottom", "top", or two-element numeric vector).
  "none" is no legend.}

  \item{defaultTheme}{plot theme to use, default is
  \code{theme_bw}}

  \item{themeExtra}{any additional \code{ggplot2} theme
  attributes to add}
}
\description{
  createMap is a smart function that, if necessary,
  geocodes data, estimates map location, queries for the
  map, places data artifacts on the map, and plots map and
  the data as shapes sized and colored based on the metrics
  specified.
}
\examples{
\donttest{
data = compute(asterConn, "pitching",
               columns = c("name || ', ' || park teamname", "lgid", "teamid", "decadeid"),
               aggregates = c("min(name) name", "min(park) park", "avg(rank) rank", "avg(attendance) attendance")
               )

geocodeMem = memoise(geocode)

createMap(data=data[data$decadeid>=2000,], source = "stamen", maptype = "watercolor", zoom=4,
              facet=c("lgid", "decadeid"),
              locationName='teamname', locationNameBak='park', metricName='attendance', labelName='name',
              shapeColour="blue", scaleRange = c(2,12), textColour="black",
              title='Game Attendance by Decade and League (yearly, 2000-2012)',
              geocodeFun=geocodeMem)
}
}

