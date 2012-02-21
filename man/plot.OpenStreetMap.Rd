\name{plot.OpenStreetMap}
\alias{plot.OpenStreetMap}
\title{plot the map in mercator coordinates. see osm().}
\usage{
  \method{plot}{OpenStreetMap} (x, y = NULL, add = FALSE,
    removeMargin = FALSE, ...)
}
\arguments{
  \item{x}{the OpenStreetMap}

  \item{y}{ignored}

  \item{add}{add to current plot}

  \item{removeMargin}{remove margins from plotting device}

  \item{...}{additional parameters to be passed to plot}
}
\description{
  plot the map in mercator coordinates. see osm().
}
\examples{
\dontrun{
library(rgdal)
m <- c(25.7738889,-80.1938889)
j <- c(58.3019444,-134.4197222)
miami <- projectMercator(25.7738889,-80.1938889)
jun <- projectMercator(58.3019444,-134.4197222)
data(states)
map <- openmap(j,m,4)
plot(map,removeMargin=TRUE)
plot(states,add=TRUE)

data(LA_places)
longBeachHarbor <- openmap(c(33.760525217369974,-118.22052955627441),
		c(33.73290566922855,-118.17521095275879),14,'bing')
coords <- coordinates(LA_places)
x <- coords[,1]
y <- coords[,2]
txt <- slot(LA_places,"data")[,'NAME']
plot(longBeachHarbor,removeMargins=TRUE,raster=TRUE)
points(x,y,col="red")
text(x,y,txt,col="white",adj=0)

 library(UScensus2000)

 lat <- c(43.834526782236814,30.334953881988564)
 lon <- c(-131.0888671875  ,-107.8857421875)
 southwest <- openmap(c(lat[1],lon[1]),c(lat[2],lon[2]),5,'osm')
 data(california.tract)
 california.tract <- spTransform(california.tract,osm())

 plot(southwest,removeMargin=TRUE)
 plot(california.tract,add=TRUE)

}
}

