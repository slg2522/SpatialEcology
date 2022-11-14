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
library(palr)
library(dplyr)
library(sp)

# set path and filename
files<- list.files("D:/Cold Pool/1993",pattern='*.nc',full.names=TRUE)
path <- "D:/Cold Pool/1993/"
dname <- "TEMP"

#extent
Ext <- extent(c(222.6804, 584.4472, 109.7872, 349.5629))

# rasterVis plot
library(RColorBrewer)
rbcols <- brewer.pal(11, 'RdBu')
newcol <- colorRampPalette(rbcols)
ncols <- 1000
rbcols2 <- newcol(ncols)#apply the function to get 1000 colours

mapTheme <- rasterTheme(region = rev(rbcols2))
cutpts <- c(seq(-2, 16, by=0.025))

raster_list = list()
plot_list = list()
months_list = list("-01", "-02", "-03", "-04", "-05", "-06", "-07", "-08", "-09", "-10", "-11", "-12")

setwd("D:/Cold Pool/1993")

#loop through files
for (i in seq_along(files)) {
  nc <- nc_open(files[i])
  ncfname <- files[i]
  #print(nc)
  
  #raster brick for temperature
  tmp_raster <- brick(ncfname, varname=dname)
  tmp_raster; class(tmp_raster)
  #crs
  crs(tmp_raster) <- "EPSG:3573"
  #crop the outline
  tmp_raster.crop <- crop(tmp_raster, drawExt2)
  
  #label
  label <- toString(ncatt_get(nc,0,"title"))
  label <- as.list(strsplit(label, "TRUE, ")[[1]])
  title <- label[2]
  filetitle <- paste(title, months_list[i], ".tif", sep="")
  
  #find the minimum temperature and plot
  minTemp <- calc(tmp_raster.crop, min, na.rm=TRUE, forcefun=FALSE, forceapply=FALSE, colNA='black')
  
  # Not sure if these are needed, but match the official example
  options = c("COMPRESS=LZW", "BIGTIFF=YES", "TILED=TRUE", "BLOCKXSIZE=256",
              "BLOCKYSIZE=256")
  
  rgb0 <- image_raster(minTemp, col = rev(rbcols2))
  
  writeRaster(rgb0, 
              filename=file.path(getwd(), filetitle), 
              datatype='INT1U',
              NAflag=0,
              options=options,
              overwrite=TRUE)

  raster_list[[i]] = minTemp
  pltMin <- levelplot(minTemp, margin = F,  at=cutpts, cuts=101, pretty=TRUE, par.settings = mapTheme,
                      main=title)
  plot_list[[i]] = pltMin
  nc_close(nc)
}

dev.off()

# Save plots to tiff. Makes a separate file for each plot.
for (i in 1:length(plot_list)) {
  file_name = paste(path, "1993", months_list[i], ".tiff", sep="")
  tiff(file_name)
  print(plot_list[[i]])
  dev.off()
}


plot(trueColorRaster)
trueColorRaster <- image_raster(trueColorRaster, col = rev(rbcols2))ster(paste0(getwd(), "/", filetitle2))
trueColorRaster

plotRGB(rgb0)trueColorRaster
boundary <- rasterToPolygons(trueColorRaster, fun=function(x){x<2 & x> -1})
coldPool <- rasterToPolygons(minTemp, fun=function(x){x< -1})
#get resolutitrueColorRaster
#merge shapes
coldPoolShape <- readOGR("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Cold Pool/Recolorize/coldPoolShape/coldPoolShape.shp")
boundaryShape <- readOGR("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Cold Pool/Recolorize/boundaryShape/boundaryShape.shp")
plot(coldPoolShape, col="black")
plot(boundaryShape, col="black")
#show together to make sure crD:/Cold Pool/1993/
together <- rbind(coldPoolShapD:/Cold Pool/1993/
plot(together, col="black")

head(coldPoolShape)D:/Cold Pool/1993/coldPoolShape.shp
head(boundaryShape)D:/Cold Pool/1993/boundaryShape.shp
identical(coldPoolShape,boundaryShape)

#calculate the areas
coldPoolArea <- sum(area(coldPoolShape))
boundaryArea <- sum(area(boundaryShape))
differenceArea <- boundaryArea - coldPoolArea

#show that the combined plot area is the same as the sum of individual areas,
#which means the resolution is constant
togetherArea <- sum(area(together))
sumArea <- boundaryArea + coldPoolArea

#express the coldpool as a percent of the total cold area
percentColdPool <- (coldPoolArea/sumArea)*100