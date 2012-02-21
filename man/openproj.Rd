\name{openproj}
\alias{openproj}
\title{Projects the open street map to an alternate coordinate system}
\usage{
  openproj(x, projection = "+proj=longlat", ...)
}
\arguments{
  \item{x}{an OpenStreetMap object}

  \item{projection}{a proj4 character string or CRS object}

  \item{...}{additional parameters for projectRaster}
}
\description{
  Projects the open street map to an alternate coordinate
  system
}
\examples{
\dontrun{
library(rgdal)
library(maps)

#plot map in native mercator coords
map <- openmap(c(70,-179),
		c(-70,179),zoom=2,type='bing')
plot(map)

#using longlat projection lets us combine with the maps library
map_longlat <- openproj(map)
plot(map_longlat,raster=TRUE)
map("world",col="red",add=TRUE)

#robinson projection. good for whole globe viewing.
map_robinson <- openproj(map_longlat, projection=
				"+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
plot(map_robinson)


map <- openmap(c(70,-179),
		c(40,179),zoom=2,type='bing')
map_longlat <- openproj(map)
#Lambert Conic Conformal (takes some time...)
map_llc <- openproj(map_longlat, projection=
				"+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96")
plot(map_llc,raster=TRUE)
#add choropleth
data(states)
st_llc <- spTransform(states,CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96"))
plot(st_llc,add=T,col=heat.colors(48,.4)[slot(st_llc,"data")[["ORDER_ADM"]]])

}
}

