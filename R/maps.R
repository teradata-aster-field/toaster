#' Grap a map, display it, and place data artifacts on it.
#' 
#' createMap is a smart function which queries for a map at a certain location or that fits data artifacts,
#' displays it and places artifacts from data on it.
#' 
#' @param data data frame with artifacts and their locations and metric(s) to be placed on the map. If location name is
#'   provided (with \code{locationName}) then it is used to gecode artifacts first. If not location then longitude and 
#'   latitude must be provided. It is caller's responsibility adjust locations with value of \code{zoom} parameter to
#'   fit artifacts on the map.
#' @param maptype map theme as defined in \code{\link{get_map}}. options available are 'terrain', 
#'   'satellite', 'roadmap', and 'hybrid'
#' @param mapColor color (\code{'color'}) or black-and-white (\code{'bw'})
#' @param source Google Maps ('google'), OpenStreetMap ('osm'), Stamen Maps ('stamen'), or 
#'   CloudMade maps ('cloudmade')
#' @param location location of the map: longitude/latitude pair (in that order), or left/bottom/right/top bounding 
#'   box: 'center' uses 2 value vector for the center of the map, while 'box' uses 4 value vector as left/bottom/right/top. 
#'   If missing then function will use parameter \code{locator} and \code{data} to determine map location.
#' @param locator in absence of \code{location} specifies how to use data to determine map location: 
#'   when 'center' then function averages out data point longitude and latitude values to get approximate cneter for the 
#'   map; when 'box' it will use min/max of longitude and latitude values to determine bounding box: left/bottom/right/top. 
#'   If parameter \code{locationName} is specified then function will geocode values from this column first. 
#'   If paramter \code{locationName} is missing then it assumes that data is already geocoded and stored in the columns 
#'   with the names \code{lonName} and \code{latName}.
#' @param boxBorderMargin margin size in percent of box sizes to increase box when computed from data locations.
#' @param zoom map zoom as defined in \code{\link{get_map}}: an integer from 3 (continent) to 21 (building), 
#'   default value 10 (city). Properly setting \code{zoom} for each map is responsibility of a caller. Zoom is optional
#'   when using bounding box location specification. 
#' @param locationName name of the column with strings to be geocoded to determine longitude and latitude for each data 
#'   point. If this value is specified then parameters \code{lonName} and \code{latName} are ignored.
#' @param lonName name of the column with longitude value. This value (in combination with value from column \code{latName})
#'   is used to place each data point on the map. This parameter is ignored if \code{locationName} is defined. 
#' @param latName name of the column with latitude value. This value (in combination with value from column \code{lonName})
#'   is used to place each data point on the map. This parameter is ignored if \code{locationName} is defined.
#' @param metricName name of the column to use for the artifact metric when displaying data.
#' @param scaleRange a numeric vector of lenght 2 that specifies the minimum and maximum size of the
#'   plotting symbol after transformation (see @param range in \code{\link{scale_size}})
#' @param labelName name of the column to use for the artifact label when displaying data. 
#' @param shapeColour color for shape.
#' @param textColour color for text.
#' @param textFamily font family to use (when available).
#' @param textFace font style to apply to text: 'plain' (default), 'bold', 'italic', or 'bold.italic'.
#' @param geocodeFun geocode function. Default is \code{\link{geocode}} but due to Google API 
#'   restrictions use memoised version, e.g. \code{memoise(geocode)}, instead (see package \code{\link{memoise}}).
#' @param getmapFun get map function. Defayult is \code{\link{get_map}} but due to map APIs restrictions use memoised 
#'   version, e.g. \code{memose(get_map)}, instead (see package \code{\link{memoise}})
#' @param urlonly return url only
#' @param api_key an api key for cloudmade maps
#' @param legendPosition the position of legends. ("left", "right", "bottom", "top", or two-element numeric 
#'   vector). "none" is no legend.
#' @param defaultTheme plot theme to use, default is \code{theme_bw}
#' @param themeExtra any additional \code{ggplot2} theme attributes to add
#' 
#' @export  
#' 
#' @examples
#' \donttest{
#' data = compute(asterConn, "pitching",
#'                columns = c("name || ', ' || park teamname", "lgid", "teamid", "decadeid"),
#'                aggregates = c("min(name) name", "min(park) park", "avg(rank) rank", "avg(attendance) attendance")
#'                )
#'                
#' geocodeMem = memoise(geocode)
#' 
#' createMap(data=data[data$decadeid>=2000,], source = "stamen", maptype = "watercolor", zoom=4, 
#'               facet=c("lgid", "decadeid"),
#'               locationName='teamname', locationNameBak='park', metricName='attendance', labelName='name',
#'               shapeColour="blue", scaleRange = c(2,12), textColour="black",
#'               title='Game Attendance by Decade and League (yearly, 2000-2012)',
#'               geocodeFun=geocodeMem)
#' }
createMap <- function(data,  
                      maptype = "terrain", 
                      mapColor = c("color", "bw"), 
                      source = c("google", "osm", "stamen", "cloudmade"),
                      location = NULL, locator = 'center', boxBorderMargin = 10,
                      zoom = NULL,
                      locationName = NULL, 
                      lonName = "LONGITUDE", latName = "LATITUDE",
                      facet = NULL, ncol = 1, facetScales = "fixed",
                      metricName = NULL, labelName = NULL, 
                      scaleRange = c(1,6),
                      shapeColour = "gold2", 
                      textColour = "black", textFamily='mono' , textFace="plain", textSize=4,
                      geocodeFun = memoise(geocode), getmapFun = get_map,
                      urlonly = FALSE, api_key = NULL,  
                      baseSize = 12, baseFamily = "sans", 
                      title = NULL,
                      legendPosition = "right",
                      defaultTheme = theme_bw(base_size = baseSize),
                      themeExtra = NULL) {
  
  # match argument values
  maptype = match.arg(maptype, c('terrain', 'satellite', 'roadmap', 'hybrid', 'watercolor', 'toner'))
  mapColor = match.arg(mapColor, c('color', 'bw'))
  source = match.arg(source, c("google", "osm", "stamen", "cloudmade"))
  locator = match.arg(locator, c('center', 'box'))
  
  # geocode locations
  if (missing(location)) {
    if (!missing(locationName)) {
      geocodes = ldply(data[, locationName], function(x) geocodeFun(x, output="latlon"))
    }else {
      geocodes = data[, c(lonName, latName)]
    }
    
    # calculate map location using data latitude and longitude
    if (locator == 'center') {
      # calculate center of the map
      ll = colwise(mean, na.rm = TRUE)(geocodes[, 1:2])
      location =c (ll[[1,1]], ll[[1,2]])
    }else if (locator == 'box') {
      # calculate min and max values
      mins = colwise(min, na.rm = TRUE)(geocodes[, 1:2])
      maxs = colwise(max, na.rm = TRUE)(geocodes[, 1:2])
      margin10percentLon = boxBorderMargin/100. * (maxs[1,1] - mins[1,1])
      margin10percentLat = boxBorderMargin/100. * (maxs[1,2] - mins[1,2])
      location = c(mins[1,1] - margin10percentLon, mins[1,2] - margin10percentLat, 
                      maxs[1,1] + margin10percentLon, maxs[1,2] + margin10percentLat)
    }
  }else {
    if (!typeof(location) %in% c('numeric','integer','double')) {
      stop("Parameter location is not numeric.")
    }
    
    if (!length(location) %in% c(2,4)) {
      stop("Length of parameter location must be 2 or 4.")
    }
  }
  
  # Set zoom if missing
  if (missing(zoom))
    if (source == 'google') zoom = 10
    else
      if (length(location) == 4 ) zoom = 'auto'
      else zoom = 10
  
  # normalize parameters for stamen
  if (source == 'stamen' && !(maptype %in% c('terrain', 'watercolor', 'toner'))) {
    warning(paste('Changed maptype for stamen to terrain instead of unsupported', maptype))
    maptype = 'terrain'
  }
  
  # Load map data
  m = getmapFun(location=location, zoom=zoom, scale=2, maptype=maptype, color=mapColor, source=source,
              urlonly = urlonly, api_key = api_key)
  
  if (urlonly) {
    return(m)
  }
  
  # Add geocodes to data
  if (!missing(locationName)) {
    data[, lonName] = geocodes$lon
    data[, latName] = geocodes$lat
    # remove data that didn't get geocoded successfully
    data = data[complete.cases(data[,c(lonName,latName)]),]
  }
  
  # Create map with data
  p = ggmap(m) +
    labs(title=title)
  
  if (!missing(metricName)) {
    p = p +
      geom_point(data=data, aes_string(x=lonName, y=latName, size=metricName), colour=shapeColour) +
      scale_size(metricName, range=scaleRange)
  } 
  
  if (!missing(labelName)) {
    p = p +
      geom_text(data=data, aes_string(label=labelName, x=lonName, y=latName), colour=textColour, 
                family=textFamily , face=textFace, size=textSize, hjust=0.5, vjust=-0.5)
  }
  
  p = applyFacet(p, facet, facetScales, ncol)
  
  # apply themes
  p = p +
    theme(legend.position = legendPosition, 
          plot.title = element_text(family = baseFamily, face = "bold", size = baseSize * 1.4, vjust = 1),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  return(p)
}


#' Creates map with 48 contiguous US states.
#' 
#' @param states vector of location strings to include in the map. Usually are names of states,
#'   cities, counties or mix. Each location will be geocoded against Google API and average of 
#'   resulting longitudes and lattitudes used. Use \code{zoom} and \code{scale} to fit them all
#'   on  resulting map. If you dataset has been geocoded you may want not to use it.
createUS48Map <- function() {
  
}