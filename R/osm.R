# TODO: Add comment
# 
# Author: ianfellows
###############################################################################

#' open street map (and google) mercator projection
osm <- function(){
	CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")
}

#' Latitude Longitude projection
longlat <- function(){
	CRS("+proj=longlat +datum=WGS84")
}

#'maps long lat values to the open street map mercator projection
#' @param lat a vector of latitudes
#' @param long a vector of longitudes
#' @param drop drop to lowest dimension
projectMercator <- function(lat,long,drop=TRUE){
	library(rgdal)
	df <- data.frame(long=long,lat=lat)
	coordinates(df) <- ~long+lat
	proj4string(df) <- CRS("+proj=longlat +datum=WGS84")
	df1 <- spTransform(df,osm())
	coords <- coordinates(df1)
	colnames(coords) <- c("x","y")
	if(drop)
		coords <- drop(coords)
	coords
}


#'get an open street map tile. tpe can be "osm" or "bing"
#' @param x location in osm native coordinates
#' @param y location in osm native coordinates
#' @param zoom zoom level
#' @param type osm for mapnik open street map, or 'bing' for bing aerial
#' @return a tile
osmtile <- function(x,y,zoom,type="osm"){
	x <- as.double(x)
	y <- as.double(y)
	zoom <- as.double(zoom)
	TC <- J("edu.cens.spatial.RTileController")
	res <- TC$getInstance(type)$getTileValues(x,y,zoom)
	if(is.null(res))
		stop(paste("could not obtain tile:",x,y,zoom))
	res1 <- as.character(as.hexmode(res))
	colrs <- paste("#",substring(res1,3),sep="")
	sc <- 20037508*2
	minim <- -20037508
	
	p1 <- c(x/(2^zoom)*sc+minim,-(y/(2^zoom)*sc+minim))
	p2 <- c((x+1)/(2^zoom)*sc+minim,-((y+1)/(2^zoom)*sc+minim))
	bbox <- list(p1=p1,p2=p2)
	res <- list(colorData=colrs,bbox=bbox,projection=osm(),xres=255,yres=255)
	class(res) <- "osmtile"
	res
}

#'add tile to plot
#' @param x the tile
#' @param y ignored
#' @param add add to current plot (if raster, then image is always added)
#' @param raster use raster image
#' @param ... additional parameters to image or rasterImage
#' @method plot osmtile
plot.osmtile <- function(x, y=NULL, add=TRUE, raster=FALSE, ...){
	xres <- x$xres
	yres <- x$yres
	if(!raster)
		image(x=seq(x$bbox$p1[1] - .5*abs(x$bbox$p1[1]-x$bbox$p2[1])/yres,
			x$bbox$p2[1] + .5*abs(x$bbox$p1[1]-x$bbox$p2[1])/yres,length=yres),
			y=seq(x$bbox$p2[2] - .5*abs(x$bbox$p1[2]-x$bbox$p2[2])/xres,
				x$bbox$p1[2] + .5*abs(x$bbox$p1[2]-x$bbox$p2[2])/xres,length=xres),
			z=t(matrix(1:(xres*yres),nrow=xres,byrow=TRUE))[,xres:1],
			col=x$colorData,add=add,...)
	else
		rasterImage(as.raster(matrix(x$colorData,nrow=xres,byrow=TRUE)),
				x$bbox$p1[1] - .5*abs(x$bbox$p1[1]-x$bbox$p2[1])/yres,
				x$bbox$p2[2] + .5*abs(x$bbox$p1[2]-x$bbox$p2[2])/xres,
				x$bbox$p2[1] - .5*abs(x$bbox$p1[1]-x$bbox$p2[1])/yres,
				x$bbox$p1[2] + .5*abs(x$bbox$p1[2]-x$bbox$p2[2])/xres,
				...)
}

#' get a map based on lat long coordinates 
#' @param upperLeft the upper left lat and long
#' @param lowerRight the lower right lat and long
#' @param zoom the zoom level. If null, it is determined automatically
#' @param type 'osm' for mapnik open street map, or 'bing' for bing aerial
#' @param minNumTiles If zoom is null, zoom will be chosen such that
#' 					the number of map tiles is greater than or equal 
#' 					to this number.
#' @examples \dontrun{
#' #Korea
#' map <- openmap(c(43.46886761482925,119.94873046875),
#' 				c(33.22949814144951,133.9892578125),type='osm')
#' plot(map,raster=TRUE)
#' }
openmap <- function(upperLeft,lowerRight,zoom=NULL,type="osm",minNumTiles=9L){
	autoZoom <- is.null(zoom)
	if(autoZoom)
		zoom <- 1L
	else
		zoom <- as.integer(zoom)
	ts <- new(J("org.openstreetmap.gui.jmapviewer.tilesources.BingAerialTileSource"))
	for(i in 1:18){
		minY <-as.integer(floor(ts$latToTileY(upperLeft[1],zoom)))
		maxY <-as.integer(floor(ts$latToTileY(lowerRight[1],zoom)))
	
		minX <-as.integer(floor(ts$lonToTileX(upperLeft[2],zoom)))
		maxX <-as.integer(floor(ts$lonToTileX(lowerRight[2],zoom)))
		ntiles <- (maxX-minX+1)*(maxY-minY+1)
		if(!autoZoom)
			break
		if(ntiles>=minNumTiles)
			break
		else
			zoom <- as.integer(zoom + 1L)
	}
	map <- list(tiles=list())
	for( x in minX:maxX){
		for(y in minY:maxY){
			tile <- osmtile(x,y,zoom,type)
			map$tiles[[length(map$tiles)+1]] <- tile
		}
	}
	map$bbox <- list(p1=projectMercator(upperLeft[1],upperLeft[2]),p2=projectMercator(lowerRight[1],lowerRight[2]))
	class(map) <- "OpenStreetMap"
	attr(map,"zoom") <- zoom
	map
}

#'plot the map in mercator coordinates. see osm().
#' @param x the OpenStreetMap
#' @param y ignored
#' @param add add to current plot
#' @param removeMargin remove margins from plotting device
#' @param ... additional parameters to be passed to plot
#' @method plot OpenStreetMap
#' @examples \dontrun{
#' library(rgdal)
#' m <- c(25.7738889,-80.1938889)
#' j <- c(58.3019444,-134.4197222)
#' miami <- projectMercator(25.7738889,-80.1938889)
#' jun <- projectMercator(58.3019444,-134.4197222)
#' data(states)
#' map <- openmap(j,m,4)
#' plot(map,removeMargin=TRUE)
#' plot(states,add=TRUE)
#' 
#' data(LA_places)
#' longBeachHarbor <- openmap(c(33.760525217369974,-118.22052955627441),
#' 		c(33.73290566922855,-118.17521095275879),14,'bing')
#' coords <- coordinates(LA_places)
#' x <- coords[,1]
#' y <- coords[,2]
#' txt <- slot(LA_places,"data")[,'NAME']
#' plot(longBeachHarbor,removeMargins=TRUE,raster=TRUE)
#' points(x,y,col="red")
#' text(x,y,txt,col="white",adj=0)
#' 
#'  library(UScensus2000)
#'  
#'  lat <- c(43.834526782236814,30.334953881988564)
#'  lon <- c(-131.0888671875  ,-107.8857421875)
#'  southwest <- openmap(c(lat[1],lon[1]),c(lat[2],lon[2]),5,'osm')
#'  data(california.tract)
#'  california.tract <- spTransform(california.tract,osm())
#'  
#'  plot(southwest,removeMargin=TRUE)
#'  plot(california.tract,add=TRUE)
#' 
#' }
plot.OpenStreetMap <- function(x,y=NULL,add=FALSE,removeMargin=FALSE, ...){
	if(add==FALSE){
		plot.new()
		if(removeMargin)
			par(mar=c(0,0,0,0))
		plot.window(xlim=c(x$bbox$p1[1],x$bbox$p2[1]),ylim=c(x$bbox$p2[2],x$bbox$p1[2]) ,
				xaxs = 'i', yaxs = 'i', asp=T)
	}
	for(tile in x$tiles)
		plot(tile,...)
}





#' create a RasterLayer from a tile
#' @param x an osmtile
#' @param ... unused
setMethod("raster","osmtile",function(x, ...){
	library(raster)
	rgbCol <- col2rgb(x$colorData)
	
	red <- matrix(rgbCol[1,],nrow=x$xres,byrow=TRUE)
	green <- matrix(rgbCol[2,],nrow=x$xres,byrow=TRUE)
	blue <- matrix(rgbCol[3,],nrow=x$xres,byrow=TRUE)
	
	xmn <- x$bbox$p1[1]
	xmx <- x$bbox$p2[1]
	ymn <- x$bbox$p2[2]
	ymx <- x$bbox$p1[2]

	ras <- stack(raster(red,xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx),
			raster(green,xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx),
			raster(blue,xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx))
	projection(ras) <- x$projection	
	ras
}
)

#' create a RasterLayer from an OpenStreetMap
#' @param x an OpenStreetMap
#' @param ... unused
setMethod("raster","OpenStreetMap",function(x, ...){
	rasterImg <- NULL
	for(i in 1:length(x$tiles)){
		if(i==1)
			rasterImg <- raster(x$tiles[[i]])
		else
			rasterImg <- raster::merge(rasterImg,raster(x$tiles[[i]]))
	}	
	rasterImg
}
)

#' Projects the open street map to an alternate coordinate system
#' @param x an OpenStreetMap object
#' @param projection a proj4 character string or CRS object
#' @param ... additional parameters for projectRaster
#' @examples \dontrun{
#' library(rgdal)
#' library(maps)
#' 
#' #plot map in native mercator coords
#' map <- openmap(c(70,-179),
#' 		c(-70,179),zoom=2,type='bing')
#' plot(map)
#' 
#' #using longlat projection lets us combine with the maps library
#' map_longlat <- openproj(map)
#' plot(map_longlat,raster=TRUE)
#' map("world",col="red",add=TRUE)
#' 
#' #robinson projection. good for whole globe viewing.
#' map_robinson <- openproj(map_longlat, projection=
#' 				"+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#' plot(map_robinson)			
#' 
#' 
#' map <- openmap(c(70,-179),
#' 		c(40,179),zoom=2,type='bing')
#' map_longlat <- openproj(map)
#' #Lambert Conic Conformal (takes some time...)
#' map_llc <- openproj(map_longlat, projection=
#' 				"+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96")
#' plot(map_llc,raster=TRUE)
#' #add choropleth
#' data(states)
#' st_llc <- spTransform(states,CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96"))
#' plot(st_llc,add=T,col=heat.colors(48,.4)[slot(st_llc,"data")[["ORDER_ADM"]]])
#' 
#' }
openproj <- function(x,projection = "+proj=longlat",...){
	if(!is.character(projection))
		projection <- projection@projargs
	
	rasterImg <- raster(x)
	ras2 <- projectRaster(rasterImg,crs=projection,...)
	
	vals <- values(ras2)
	vals <- pmin(pmax(vals,0L),255L)
	flag <- apply(vals,1,function(a)any(!is.finite(a)))
	vals1 <- vals
	vals1[!is.finite(vals)] <- 0L
	colors <- ifelse(flag,NA,rgb(vals1[,1],vals1[,2],vals1[,3],maxColorValue=255L))
	ext <- extent(ras2)
	
	result <- list()
	result$colorData <- colors
	result$bbox <- list(p1 = c(ext@xmin,ext@ymax), p2 = c(ext@xmax,ext@ymin))
	result$projection <- CRS(projection)
	result$xres <- dim(ras2)[1]
	result$yres <- dim(ras2)[2]
	class(result) <- "osmtile"
	
	osm <- list(tiles=list(result))
	osm$bbox <- result$bbox
	attr(osm,"zoom") <- attr(x,"zoom")
	class(osm) <- "OpenStreetMap"
	osm
}


#'print map
#' @param x the OpenStreetMap
#' @param ... ignored
#' @method print OpenStreetMap
print.OpenStreetMap <- function(x,...){
	print(str(x))
}

