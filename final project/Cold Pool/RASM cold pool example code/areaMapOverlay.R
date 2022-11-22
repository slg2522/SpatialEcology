library(tidycensus)
library(dplyr)
library(ggplot2)
library(plotly)
library(sf)
library(raster)
library(rasterVis)
library(purrr)
library(R.utils)
library(gridExtra)
library(stringr)
library(palr)
library(dplyr)
library(sp)
library(rgdal)
library(gdata)

setwd("D:/Cold Pool/coldPool/areaGraphs")
path <- "D:/Cold Pool/coldPool/areaGraphs/"


#set up list of shapes
shapes <- list.files("D:/Cold Pool/1980",pattern='*.shp',full.names=TRUE)
shape_list <- c("p1980.jan", "p1980.feb", "p1980.mar", "p1980.apr", "p1980.may", "p1980.jun", "p1980.jul", "p1980.aug", "p1980.sep", "p1980.oct", "p1980.nov", "p1980.dec")
color_list <- c("red", "orange", "yellow", "green", "blue", "purple", "pink", "grey", "brown", "cyan", "gold", "tan")

#read in 1980
for (i in seq_along(shapes)) {
  #read in shape
  shp <- readOGR(shapes[i])
  #plot shape
  plot(shp, col=color_list[i], main=shape_list[i])
  #check the CRS
  crs(shp)
  #rename the shape
  mv(from="shp", to=shape_list[i])
}

p1980.jan$layer.2


#overlap all of them just to show that they maintain coordinates (ie visual of "crs(pool1)")
together <- rbind(pool1, pool2, pool3, pool4, pool5, pool6, pool7, pool8, pool9, pool10, pool11, pool12)
plot(together)

# Merging cells into shapes
a1980.jan <- gUnion(p1980.jan, p1980.jan, byid = FALSE)
a1980.feb <- gUnion(p1980.feb, p1980.feb, byid = FALSE)
a1980.mar <- gUnion(p1980.mar, p1980.mar, byid = FALSE)
a1980.apr <- gUnion(p1980.apr, p1980.apr, byid = FALSE)
a1980.may <- gUnion(p1980.may, p1980.may, byid = FALSE)
a1980.jun <- gUnion(p1980.jun, p1980.jun, byid = FALSE)
a1980.jul <- gUnion(p1980.jul, p1980.jul, byid = FALSE)
a1980.aug <- gUnion(p1980.aug, p1980.aug, byid = FALSE)
a1980.sep <- gUnion(p1980.sep, p1980.sep, byid = FALSE)
a1980.oct <- gUnion(p1980.oct, p1980.oct, byid = FALSE)
a1980.nov <- gUnion(p1980.nov, p1980.nov, byid = FALSE)
a1980.dec <- gUnion(p1980.dec, p1980.dec, byid = FALSE)

plot(a1980.jan, col="red")
plot(a1980.feb, col="red")
plot(a1980.mar, col="red")
plot(a1980.apr, col="red")
plot(a1980.may, col="red")
plot(a1980.jun, col="red")
plot(a1980.jul, col="red")
plot(a1980.aug, col="red")
plot(a1980.sep, col="red")
plot(a1980.oct, col="red")
plot(a1980.nov, col="red")
plot(a1980.dec, col="red")

#overlap all of them just to show that they maintain coordinates (ie visual of "crs(pool1)")
together <- gUnion(a1980.jan, a1980.feb, a1980.mar, a1980.apr, a1980.may, a1980.jun, a1980.jul, a1980.aug, a1980.sep, a1980.oct, a1980.nov, a1980.dec, byid = FALSE)
together <- gUnion(a1980.jan, a1980.feb, byid = FALSE)
together <- gUnion(together, a1980.mar, byid = FALSE)
together <- gUnion(together, a1980.apr, byid = FALSE)
together <- gUnion(together, a1980.may, byid = FALSE)
together <- gUnion(together, a1980.jun, byid = FALSE)
together <- gUnion(together, a1980.jul, byid = FALSE)
together <- gUnion(together, a1980.aug, byid = FALSE)
together <- gUnion(together, a1980.sep, byid = FALSE)
together <- gUnion(together, a1980.oct, byid = FALSE)
together <- gUnion(together, a1980.nov, byid = FALSE)
together <- gUnion(together, a1980.dec, byid = FALSE)

plot(together)
together <- spTransform(together, CRS("+init=epsg:4326"))
plot(together)
raster::shapefile(together, paste(path, "together.shp", sep=""), overwrite=TRUE)


raster::shapefile(a1980.jan, paste(path, "1980Jan.shp", sep=""), overwrite=TRUE)


mapboxToken <- "pk.eyJ1Ijoic2dyb3ZlcyIsImEiOiJja3ppcXA1aWUwcjhyMnVxa2M4bmRqNnJmIn0.Vo5rwEltAZzJekzUN9PILg"
Sys.setenv("MAPBOX_TOKEN" = mapboxToken) # for Orca

nc <- sf::st_read("D:/Cold Pool/coldPool/areaGraphs/1980Jan.shp", quiet = TRUE)
ncTransform <- sf::st_read("D:/Cold Pool/coldPool/areaGraphs/ncTransform.shp", quiet = TRUE)


ncTogether <- sf::st_read("D:/Cold Pool/coldPool/areaGraphs/together.shp", quiet = TRUE)
plot(ncTogether)


fig <- plot_mapbox(ncTogether) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6
    )
  ) 
fig <- fig %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig




# Input coordinates.
#
x <- c(7.173500, 7.172540, 7.171636, 7.180180, 7.178070, 7.177229, 7.175240, 7.181409, 7.179299)
y <- c(45.86880, 45.86887, 45.86924, 45.87158, 45.87014, 45.86923, 45.86808, 45.87177, 45.87020)
#
# Define the coordinate systems.
d <- data.frame(lon=x, lat=y)
coordinates(d) <- c("lon", "lat")
proj4string(d) <- CRS(nc)
CRS.new <- CRS("+init=epsg:4326")
# (@mdsumner points out that
#    CRS.new <- CRS("+init=epsg:2056")
# will work, and indeed it does. See http://spatialreference.org/ref/epsg/2056/proj4/.)
nc.transform <- spTransform(a1980.jan, crs(nc))
nc.transform <- spTransform(a1980.jan, CRS("+proj=utm +zone=32 +ellps=WGS84"))

#
# Plot the results.
#
par(mfrow=c(1,2))
plot(a1980.jan, axes=TRUE, main="Original lat-lon")
plot(nc.transform, axes=TRUE, main="Projected")
unclass(d.ch1903)
raster::shapefile(nc.transform, paste(path, "ncTransform.shp", sep=""), overwrite=TRUE)



library(plotly)
library(sf)

mapboxToken <- "pk.eyJ1Ijoic2dyb3ZlcyIsImEiOiJja3ppcXA1aWUwcjhyMnVxa2M4bmRqNnJmIn0.Vo5rwEltAZzJekzUN9PILg"
Sys.setenv("MAPBOX_TOKEN" = mapboxToken) # for Orca

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

fig <- plot_mapbox(nc, split=~NAME) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6
    )
  ) 
fig <- fig %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig
