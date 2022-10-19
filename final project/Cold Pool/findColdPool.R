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
library(gridExtra)
library(stringr)

# set path and filename
files<- list.files("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Test/test",pattern='*.nc',full.names=TRUE)
dname <- "TEMP"

#extent
Ext <- extent(c(259.3191, 455.6267, 137.5159, 354.8564))

# rasterVis plot
mapTheme <- rasterTheme(region = rev(brewer.pal(11, "RdBu")))
cutpts <- c(-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)

#loop through files
for (i in seq_along(files)) {
  nc <- nc_open(files[i])
  ncfname <- files[i]
  #print(nc)
  
  #raster brick for temperature
  tmp_raster <- brick(ncfname, varname=dname)
  tmp_raster; class(tmp_raster)
  #crop the outline
  tmp_raster.crop <- crop(tmp_raster, drawExt2)
  
  #label
  label <- toString(ncatt_get(nc,0,"title"))
  label <- as.list(strsplit(label, "TRUE, ")[[1]])
  title <- label[2]
  
  #find the minimum temperature and plot
  minTemp <- calc(tmp_raster.crop, min, na.rm=TRUE, forcefun=FALSE, forceapply=FALSE)
  pltMin <- levelplot(minTemp, margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                      main=title)
  return(pltMin)
  nc_close(nc)
  }

plot(tmp_raster.crop)

