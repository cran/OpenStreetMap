\name{raster}
\alias{raster}
\title{Create a RasterLayer from a tile}
\arguments{
  \item{x}{an osmtile}

  \item{...}{unused}

  \item{x}{an OpenStreetMap}

  \item{...}{unused}
}
\description{
  Create a RasterLayer from a tile

  Create a RasterLayer from an OpenStreetMap
}
\examples{
\dontrun{
longBeachHarbor <- openmap(c(33.760525217369974,-118.22052955627441),
		c(33.73290566922855,-118.17521095275879),14,'bing')
ras <- raster(longBeachHarbor)
plotRGB(ras)
}
}

