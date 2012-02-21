\name{openmap}
\alias{openmap}
\title{get a map based on lat long coordinates}
\usage{
  openmap(upperLeft, lowerRight, zoom = NULL, type = "osm",
    minNumTiles = 9L)
}
\arguments{
  \item{upperLeft}{the upper left lat and long}

  \item{lowerRight}{the lower right lat and long}

  \item{zoom}{the zoom level. If null, it is determined
  automatically}

  \item{type}{'osm' for mapnik open street map, or 'bing'
  for bing aerial}

  \item{minNumTiles}{If zoom is null, zoom will be chosen
  such that the number of map tiles is greater than or
  equal to this number.}
}
\description{
  get a map based on lat long coordinates
}
\examples{
\dontrun{
#Korea
map <- openmap(c(43.46886761482925,119.94873046875),
				c(33.22949814144951,133.9892578125),type='osm')
plot(map,raster=TRUE)
}
}

