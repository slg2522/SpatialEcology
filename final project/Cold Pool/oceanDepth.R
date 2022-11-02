# load packages  
library(sf) 
library(ncdf4)
library(raster)
library(rasterVis)
library(RColorBrewer)
library(ncdf4)
library(gunzip)
library(purrr)
library(R.utils)
library(palr)

# set path and filename
ncpath <- "C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Test/"
ncname <- "R2200aRBRZBGCcaaa03a.pop.h.2016-08.nc"  
ncfname <- paste(ncpath, ncname, sep="")
dname <- "HU" #depth at point U

#check that the extents overlap

#open netcdf file
#file.choose() allows user to choose file from system
nc <- nc_open(ncfname)
#print metadata for netcdf file
print(nc)

# get temperature
depth_array <- ncvar_get(nc,dname)
dlname <- ncatt_get(nc,dname,"long_name")
dunits <- ncatt_get(nc,dname,"units")
fillvalue <- ncatt_get(nc,dname,"_FillValue")
dim(tmp_array)

# get global attributes
title <- ncatt_get(nc,0,"title")
institution <- ncatt_get(nc,0,"institution")
datasource <- ncatt_get(nc,0,"source")
references <- ncatt_get(nc,0,"references")
history <- ncatt_get(nc,0,"history")
Conventions <- ncatt_get(nc,0,"Conventions")

#Check what’s in the current workspace
ls()

#raster brick for temperature
depth_raster <- brick(ncfname, varname="HU")
depth_raster; class(tmp_raster)


plot(subset(depth_raster, 1))
depth_raster
crs(depth_raster) <- "EPSG:3573"
plot(depth_raster)

# click twice on the map to select the region of interest
#drawExt <- drawExtent()    
#drawExt
#> drawExt
#class      : Extent 
#xmin       : 222.6804 
#xmax       : 584.4472 
#ymin       : 109.7872 
#ymax       : 349.5629 
drawExt <- extent(c(222.6804, 584.4472, 109.7872, 349.5629))

#crop the outline
depth_raster.crop <- crop(depth_raster, drawExt)
#plot the new outline
plot(subset(depth_raster.crop, 1))
# click twice on the map to select the region of interest
#drawExt1 <- drawExtent()    
#drawExt1
#> drawExt1
#class      : Extent 
#xmin       : 259.3191 
#xmax       : 455.6267 
#ymin       : 137.5159 
#ymax       : 354.8564
drawExt1 <- extent(c(259.3191, 455.6267, 137.5159, 354.8564))

depth_raster.crop2 <- crop(depth_raster, drawExt1)

#plot the new outline
plot(subset(depth_raster.crop2, 1))

# click twice on the map to select the region of interest
#drawExt2 <- drawExtent()    
#drawExt2
#> drawExt1
#class      : Extent 
#xmin       : 259.3191 
#xmax       : 455.6267 
#ymin       : 137.5159 
#ymax       : 354.8564 
drawExt2 <- extent(c(259.3191, 455.6267, 137.5159, 354.8564))

#crop the outline
depth_raster.crop3 <- crop(depth_raster, drawExt2)

#plot the new outline
plot(subset(depth_raster.crop3, 1))



# rasterVis plot
mapTheme <- rasterTheme(region = brewer.pal(9, "Blues"))
cutpts <- c(500,1000,2500,5000,7500,10000,50000,100000,150000,200000,250000,300000,350000,400000,450000,500000,550000,600000)

setwd("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Cold Pool/Recolorize")

tiff("oceanDepth.tiff")
#add below for specification: at=cutpts, cuts=11,
levelplot(subset(depth_raster.crop3, 1), margin = F,  at=cutpts, cuts=10, pretty=TRUE, par.settings = mapTheme,
          main="Ocean Depth")
dev.off()

# Not sure if these are needed, but match the official example
options = c("COMPRESS=LZW", "BIGTIFF=YES", "TILED=TRUE", "BLOCKXSIZE=256", "BLOCKYSIZE=256")


## apply the colours used by raster plot() itself
odepth_raster <- image_raster(depth_raster.crop3, col = brewer.pal(9, "Blues"))
plotRGB(odepth_raster)

#write correct color raster
filetitle2 <- (paste0("oceanDepth.tif", sep=""))
writeRaster(odepth_raster, 
            filename=file.path(getwd(), filetitle2), 
            datatype='INT1U',
            NAflag=0,
            options=options,
            overwrite=TRUE)

trueColorRaster <- raster(paste0(getwd(), "/", filetitle2))
plot(trueColorRaster)