\name{osmtile}
\alias{osmtile}
\title{get an open street map tile. tpe can be "osm" or "bing"}
\usage{
  osmtile(x, y, zoom, type = "osm")
}
\arguments{
  \item{x}{location in osm native coordinates}

  \item{y}{location in osm native coordinates}

  \item{zoom}{zoom level}

  \item{type}{osm for mapnik open street map, or 'bing' for
  bing aerial}
}
\value{
  a tile
}
\description{
  get an open street map tile. tpe can be "osm" or "bing"
}

