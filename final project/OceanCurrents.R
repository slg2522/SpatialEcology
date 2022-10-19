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

# set path and filename
ncpath <- "C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Test/"
ncname <- "R2200aRBRZBGCcaaa03a.pop.h.2016-08.nc"  
ncfname <- paste(ncpath, ncname, sep="")
dname <- "Depth"  # note: tmp means temperature (not temporary)

#open netcdf file
#file.choose() allows user to choose file from system
nc <- nc_open(ncfname)
#print metadata for netcdf file
print(nc)

# get temperature
tmp_array <- ncvar_get(nc,dname)
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
tmp_raster <- brick(ncfname, varname="TEMP")
tmp_raster; class(tmp_raster)


plot(subset(tmp_raster, 1), theme = mapTheme)
# click twice on the map to select the region of interest
drawExt <- drawExtent()    
drawExt
#> drawExt
#class      : Extent 
#xmin       : 222.6804 
#xmax       : 584.4472 
#ymin       : 109.7872 
#ymax       : 349.5629 

#crop the outline
tmp_raster.crop <- crop(tmp_raster, drawExt)
#plot the new outline
plot(subset(tmp_raster.crop, 1))
# click twice on the map to select the region of interest
drawExt1 <- drawExtent()    
drawExt1
#> drawExt1
#class      : Extent 
#xmin       : 259.3191 
#xmax       : 455.6267 
#ymin       : 137.5159 
#ymax       : 354.8564 
tmp_raster.crop2 <- crop(tmp_raster, drawExt1)

#plot the new outline
plot(subset(tmp_raster.crop2, 1))

# click twice on the map to select the region of interest
drawExt2 <- drawExtent()    
drawExt2
#> drawExt1
#class      : Extent 
#xmin       : 259.3191 
#xmax       : 455.6267 
#ymin       : 137.5159 
#ymax       : 354.8564 
#crop the outline
tmp_raster.crop3 <- crop(tmp_raster, drawExt2)

#plot the new outline
plot(subset(tmp_raster.crop3, 1))



# rasterVis plot
mapTheme <- rasterTheme(region = rev(brewer.pal(11, "RdBu")))
cutpts <- c(-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
#add below for specification: at=cutpts, cuts=11,
plt1 <- levelplot(subset(tmp_raster.crop3, 1), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                 main="Temperature Depth 1")
plt1
plt5 <- levelplot(subset(tmp_raster.crop3, 5), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                  main="Temperature Depth 5")
plt5
plt7 <- levelplot(subset(tmp_raster.crop3, 7), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                  main="Temperature Depth 7")
plt7
plt9 <- levelplot(subset(tmp_raster.crop3, 9), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                  main="Temperature Depth 9")
plt9
plt11 <- levelplot(subset(tmp_raster.crop3, 11), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                   main="Temperature Depth 11")
plt11
plt13 <- levelplot(subset(tmp_raster.crop3, 13), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                   main="Temperature Depth 13")
plt13
plt15 <- levelplot(subset(tmp_raster.crop3, 15), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                   main="Temperature Depth 15")
plt15
plt17 <- levelplot(subset(tmp_raster.crop3, 17), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                   main="Temperature Depth 17")
plt17
plt19 <- levelplot(subset(tmp_raster.crop3, 19), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                   main="Temperature Depth 19")
plt19
plt21 <- levelplot(subset(tmp_raster.crop3, 21), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                   main="Temperature Depth 21")
plt21
plt23 <- levelplot(subset(tmp_raster.crop3, 23), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                   main="Temperature Depth 23")
plt23
plt25 <- levelplot(subset(tmp_raster.crop3, 25), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                   main="Temperature Depth 25")
plt25
plt27 <- levelplot(subset(tmp_raster.crop3, 27), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                   main="Temperature Depth 27")
plt27
plt29 <- levelplot(subset(tmp_raster.crop3, 29), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                   main="Temperature Depth 29")
plt29
plt31 <- levelplot(subset(tmp_raster.crop3, 31), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                   main="Temperature Depth 31")
plt31
plt33 <- levelplot(subset(tmp_raster.crop3, 33), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                   main="Temperature Depth 33")
plt33
plt35 <- levelplot(subset(tmp_raster.crop3, 35), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                   main="Temperature Depth 35")
plt35
plt37 <- levelplot(subset(tmp_raster.crop3, 37), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                   main="Temperature Depth 37")
plt37
plt39 <- levelplot(subset(tmp_raster.crop3, 39), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                   main="Temperature Depth 39")
plt39
plt41 <- levelplot(subset(tmp_raster.crop3, 41), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                   main="Temperature Depth 41")
plt41
plt44 <- levelplot(subset(tmp_raster.crop3, 44), margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                   main="Temperature Depth 44")
plt44

#grid of all the temperature at depth map plots incrementally
library(gridExtra)
grid.arrange(plt1, plt5, plt7, plt11, plt15, plt19, plt23, plt27, plt31, plt35, plt39, plt44, ncol=4, nrow=3)

#cell-level operations
# S4 method for Raster,function
minTemp <- calc(tmp_raster.crop3, min, na.rm=TRUE, forcefun=FALSE, forceapply=FALSE)
pltMin <- levelplot(minTemp, margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                   main="Minimum Temperature")

#HU: ocean depth at U points
#HT: ocean depth at T points
#z_w_bot: depth from surface to bottom of layer
