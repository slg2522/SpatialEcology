# This tutorial is an overview of working with vector & raster data in R, with
# an emphasis on combining point vector data and climate grids. There is some 
# repetition, with the "basic" tutorial, but that is intended to help practice 
# key skills / concepts.

library(rgdal) # 'Geospatial' Data Abstraction Library ('GDAL')
library(raster) # for all things raster and more
library(dismo) # species distribution modeling and much more
library(maps) # quick plotting of countries, etc.
library(gtools) # various functions
library(rasterVis) # raster visualization methods
library(fields) # Curve / function fitting for spatial analyses
library(tcltk) #build GUIs for R interface

setwd("/Users/mfitzpatrick/code/spatialEcology-in-R/teachingScripts/Week-1/")
#setwd(tk_choose.dir())

# Working with vector point data -----------------------------------------------
# Read shapefile
swEuc <- shapefile("data/swEucalyptus.shp")
class(swEuc)
plot(swEuc)

# swEuc is a spatial object with two types of information
# 1. attribute data (i.e., species names)
# 2. spatial information (geographic coordinates and a CRS)

# Have a look at the attribute table, it is in the @data slot
head(swEuc@data)

# Check the spatial attributes
head(swEuc@coords)
swEuc@bbox
extent(swEuc) #function from 'raster' package

# Let's look at the projection
swEuc@proj4string
projection(swEuc) #function from 'raster' package

# let's get just the attribute data
swTab <- data.frame(swEuc@data)

# now all we have are attribute data, spatial information is gone
head(swTab)
class(swTab)
str(swTab)
projection(swTab)
plot(swTab$x, swTab$y) # compare to plot of swEuc
# Your data will NOT be a spatial object if you only have an Excel spreadsheet
# You'll need to make it a spatial object

# let's turn the attribute table back into a spatial object
# to do so, we need to provide the coordinates and ideally a CRS
# if you have a table with coordinates and attribute data, you can use the
?sp::coordinates
# function which columns should be treated as coordinates
coordinates(swTab) <- c("x", "y")
class(swTab)
str(swTab)
projection(swTab)
swTab@proj4string
plot(swTab)

# Next, we need to define the spatial projection of the data
# if you know the projection, you can consult 
# http://www.spatialreference.org or https://epsg.io/
# to get the projection in the 
# appropriate PROJ.4 format or EPSG code. Be careful - these resources
# can be a mess with lots of projections that you
# probably don't want to use
# In this case, we know where the data came from...so we can just assign the 
# CRS label using the original spatial object.
swTab@proj4string <- swEuc@proj4string

# project the data into a new coordinate system
# Get a new proj4string from http://www.spatialreference.org
# and give it a try
crsProj <- "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
?sp::spTransform
swTabProj <- spTransform(swTab, CRS=crsProj)
plot(swTabProj)

#write new, projected version to shapefile
outfile <- "swProj.shp"
shapefile(swTabProj, outfile, overwrite=TRUE)


# Retrieving & preparing species occurrence data from GBIF ---------------------
# Global Biodiversity Information Facility (http://gbif.org)
# Lets get data for karri (Eucalyptus diversicolor, one of the tallest 
# tree species in the world, endemic to southwestern Australia)
# gbif function from 'dismo'
?gbif
# many other ways to download species data:
# library(sppocc)
#library(rgbif)
#library(rinat)
#library(BIEN)
#library(rbison)
#library(ridigbio)
#library(neotoma)

karri <- gbif("Eucalyptus", species="diversicolor", nrecs=200, geo=F)
# note you can limit to a certain geographic extent, but we will skip that here
class(karri)
dim(karri) # 127 columns!
View(karri)
head(karri$datasetName) # many records are from iNaturalist

# let's reduce the data to a few useful columns
# but first, let's save the projection information that is
# stored in the 'geodeticDatum column
unique(karri$geodeticDatum)
crsKarri <- "WGS84"
# see ?subset = useful function
karri <- subset(karri, select=c("species", "country", "lat", "lon", "locality", 
                                "year", "coordinateUncertaintyInMeters"))

# review the attributes, duplicates, etc
dim(karri)
duplicated(karri) #lots of duplicated records - common problem with GBIF data
#View(karri[duplicated(karri),])
karri <- unique(karri)
dim(karri)

# now convert to spatial object
coordinates(karri) <- c("lon", "lat") # throws an error, NA values    
# need to deal with NA values
which(is.na(karri$lat))
which(is.na(karri$lon))

# try converting to spatial object again
# remove NAs
karri <- karri[-which(is.na(karri$lat)),]
which(is.na(karri$lon))
coordinates(karri) <- c("lon", "lat") 

map('world')
plot(karri, pch=21, bg=rgb(1,0,0,0.5), cex=1, add=T)
# what does bg=rgb(1,0,0,0,0.5) do?


# karri is endemic to southwestern Australia....
# lets use only those records within the known native range
# but first, we need to get everything in the same projection
# let's assign CRS information
crsKarri
# http://www.spatialreference.org
# geographical, datum WGS84
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")    
# define projection system of our data
crs(karri) <- crs.geo     

# polygon for sw Aus
swPoly <- shapefile("data/swPoly.shp")
plot(swPoly)
swPoly@proj4string

# We can use the 'over' function to select points that fall within
# the sw polygon, but we need to project / transform the data first
swPolyProj <- spTransform(swPoly, CRSobj=projection(karri))
karri2 <- over(karri, swPolyProj)
which(is.na(karri2)) # 29 records outside of the sw polygon
karri <- karri[-which(is.na(karri2)),] # remove the 29 records
plot(swPolyProj)
plot(karri, pch=21, bg=rgb(0,0,1,0.5), add=T)


# lets select / highlight different points based on their attributes
karri.year <- subset(karri, year<1900) # old records
karri.loc <- karri[grep("Walpole", karri$locality),] # in 'Walpole'
plot(swPolyProj)
plot(karri, pch=21, bg=rgb(0,0,1,0.5), add=T)
# near Walpole
plot(karri.loc, pch=20, col="yellow", add=T)
# old record pre-1900
plot(karri.year, pch=15, col="red", cex=2, add=T)

# which points have high uncertainty?
plot(swPolyProj)
plot(karri, pch=21, bg=rgb(0,0,1,0.5), add=T, cex=0.5)
plot(karri[na.omit(karri$coordinateUncertaintyInMeters)>10000,],
     pch=15, col=rgb(1,0,0,0.5), add=T)



# Raster data ------------------------------------------------------------------
# Downloading raster climate data from internet 
# The `getData` function from the `dismo` package will easily retrieve 
# climate data, elevation, administrative boundaries, etc.
# lets retrieve global bioclimatic variables at 10' resolution
?getData
# see also 
# library(sdmpredictors)
bioclimVars <- getData(name="worldclim", #other options available 
                       res = 10, # resolution
                       var = "bio") # which variable(s)?
class(bioclimVars) # raster stack
# A raster stack is collection of many raster layers with the same projection, 
# spatial extent and resolution. 
bioclimVars
extent(bioclimVars)
plot(bioclimVars) # takes a few seconds...plots first 16 in stack
plot(bioclimVars[[1]])

# Loading a single raster layer 
# downloaded in .bil format, comprised of two files
filePath <- paste(getwd(), "/wc10/bio19.bil", sep="")
filePath
bio19 <- raster(paste(getwd(), "/wc10/bio19.bil", sep=""))
plot(bio19)
# you can zoom to a window by clicking two locations on the plot
zoom(bio19)

# performing calculations
bio1 <- raster(paste(getwd(), "/wc10/bio1.bil", sep=""))
bio1 <- bio1/10    # Worldclim temperature data are degrees C * 10
bio1  # look at the info
plot(bio1)

# Creating a raster stack 
# Let's collect several raster files from disk 
# and read them as a single raster stack:
file.remove(paste(getwd(), "/wc10/", "bio_10m_bil.zip", sep=""))
# sort the file names using ?mixedsort
files <- list.files(path=paste(getwd(), "/wc10/", sep=""), 
                    full.names=T, 
                    pattern=".bil")
# we want to stack them in order by name (1-19), so we need to sort 
# the file paths
list.ras <- mixedsort(files)
list.ras 
# in order to stack rasters, they MUST ALL be identical in terms
# of extent, resolution, etc.
bioclimVars <- stack(list.ras)
bioclimVars

# Raster bricks 
# A rasterbrick is similar to a raster stack (i.e. multiple layers with the 
# same extent and resolution), but all the data are stored in a single 
# file on disk.
bioclim.brick <- brick(bioclimVars)   # creates rasterbrick
# can write the brick as a single file if you want:
writeRaster(bioclim.brick, "bioclim.brick.tif", overwrite=T)

# Crop rasters
# Crop raster manually by drawing region of interest
plot(bio19)
# click twice on the map to select the region of interest
drawExt <- drawExtent()    
drawExt
bio19.sw <- crop(bio19, drawExt)
plot(bio19.sw)

# Alternatively, provide coordinates for the limits of the region of interest:
coordExt <- c(110, 130, -37, -18) # Australia   
bio19.sw <- crop(bio19, coordExt)
plot(bio19.sw)

# Or crop a raster stack using a polygon extent
swPolyProj <- spTransform(swPoly, projection(bio19.sw))
polyExt <- extent(swPolyProj)
bioclimVars.sw <- crop(bioclimVars, polyExt)
plot(bioclimVars.sw)
plot(bioclimVars.sw[[1]])
plot(swPolyProj, add=T)

# or use *mask* to crop boundary of polygon
plot(mask(bio19.sw, swPolyProj))
swBioClim <- mask(bioclimVars.sw, swPolyProj)

# Changing projection
# Use `projectRaster` function:
bio19.swProj <- projectRaster(bio19.sw, crs="+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +units=m +no_defs")   # can also use a template raster, see ?projectRaster
bio19.swProj   # notice info at coord.ref.
# RECALL: projection of raster requires recalculation of grid values,
# so values will change 
ncell(bio19.sw)
ncell(bio19.swProj)
cellStats(bio19.sw, max)
cellStats(bio19.swProj, max)

# Extract values from raster 
# generate some random locations
set.seed(9032020)
coords <- cbind(x=runif(100,115,130), y=runif(100,-35,-20)) 
head(coords)
plot(bio19.sw)
points(coords)
# Use `extract` function
bio19Dat <- extract(bio19.sw, coords)
coords <- cbind(coords, bio19Dat)    # raster values
head(coords)
coords <- na.omit(coords)
# are incorporated to the dataframe
plot(bio19.sw)
points(coords[,1:2])

# extract bio19 values using buffer around points
climBuff <- extract(bio19.sw, #raster 
                    karri, #ppoints to buffer
                    cellnumbers=TRUE, #return cell numbers too?
                    buffer=100000) # buffer size in meters
class(climBuff)
length(climBuff)
head(climBuff[[1]])


# Using `rasterToPoints`:
bio19Pts <- data.frame(rasterToPoints(swBioClim$bio19))
head(bio19Pts)
dim(bio19Pts)
#use 'rasterize' to recreate raster from points & data
newRast <- rasterize(bio19Pts[,1:2], swBioClim$bio19, field=bio19Pts$bio19)
plot(newRast)

# And also, the `click` function will get values from particular locations in the map
plot(swBioClim$bio19)
# click n times in the map to get values
click(swBioClim$bio19, n=5)   

# Changing raster resolution 
# Use `aggregate` function:
bio19.swLowres <- aggregate(swBioClim$bio19, fact=4, fun=mean)
bio19.swLowres
swBioClim$bio19     # compare
par(mfcol=c(1,2))
plot(swBioClim$bio19, main="original")
plot(bio19.swLowres, main="low resolution")

# Here's a very useful trick if you have rasters that almost match, 
# but will not stack because they do not align perfectly
library(gdalUtils) # install if you don't have it
junkR <- aggregate(swBioClim$bio19, fact=2, fun=mean)
stack(junkR, swBioClim) # error
# have a look at:
?align_rasters
# Can you get them to stack?


# Elevations, slope, aspect, etc
# Download elevation data:
dev.off()
elevation <- getData('alt', country='AUS')
plot(elevation)
# I draw my extent to capture the Australian Alps, in the SE corner of AUS
elevation.c <- crop(elevation, drawExtent())
plot(elevation.c)

# Some quick maps:
slopAsp <- terrain(elevation.c, opt=c('slope', 'aspect'), unit='degrees')
par(mfrow=c(1,2))
plot(slopAsp)
dev.off()
slope <- terrain(elevation.c, opt='slope')
aspect <- terrain(elevation.c, opt='aspect')
hill <- hillShade(slope, aspect, 40, 270)
plot(hill, col=grey(0:100/100), legend=FALSE, main='Australian Alps')
plot(elevation.c, col=rainbow(25, alpha=0.35), add=TRUE)

# Saving and exporting raster data 
# `writeRaster` can export to many different file types
?writeRaster
writeRaster(bio19.sw, filename="bio19.sw.grd")   
################################################################################

