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
files<- list.files("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Cold Pool/Recolorize",pattern='*.nc',full.names=TRUE)
path <- "C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Cold Pool/Recolorize/"
dname <- "TEMP"

#extent
#Ext <- extent(c(259.3191, 455.6267, 137.5159, 354.8564))
Ext <- extent(c(222.6804, 584.4472, 109.7872, 349.5629))

# rasterVis plot
library(RColorBrewer)
rbcols <- brewer.pal(11, 'RdBu')
newcol <- colorRampPalette(rbcols)
ncols <- 1000
rbcols2 <- newcol(ncols)#apply the function to get 100 colours

mapTheme <- rasterTheme(region = rev(rbcols2))
cutpts <- c(seq(-2, 16, by=0.025))

raster_list = list()
plot_list = list()
months_list = list( "-test3", "-test4", "-test5", "-test6")

setwd("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Cold Pool/Recolorize")

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
  
  writeRaster(minTemp, 
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

## apply the colours used by raster plot() itself
rgb0 <- image_raster(minTemp, col = rev(rbcols2))
plotRGB(rgb0)
plotRGB(rgb0)

#write correct color raster
filetitle2 <- (paste0(title, "TrueColors.tif", sep=""))
writeRaster(rgb0, 
            filename=file.path(getwd(), filetitle2), 
            datatype='INT1U',
            NAflag=0,
            options=options,
            overwrite=TRUE)

trueColorRaster <- raster(paste0(getwd(), "/", filetitle2))
plot(trueColorRaster)

plotRGB(rgb0)
boundary <- rasterToPolygons(minTemp, fun=function(x){x<2 & x> -1})
coldPool <- rasterToPolygons(minTemp, fun=function(x){x< -1})
plot(boundary, add=TRUE, col='red')
plot(coldPool, add=TRUE, col='blue')

#write shapefiles
shapefile(coldPool, filename="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Cold Pool/Recolorize/coldPoolShape.shp")
shapefile(boundary, filename="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Cold Pool/Recolorize/boundaryShape.shp")

#merge shapes
coldPoolShape <- readOGR("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Cold Pool/Recolorize/coldPoolShape/coldPoolShape.shp")
boundaryShape <- readOGR("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Cold Pool/Recolorize/boundaryShape/boundaryShape.shp")
plot(coldPoolShape)



test1 <- st_read("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Cold Pool/Recolorize/coldPoolShape/coldPoolShape.shp")
st_as_sf(test1)
test <- st_combine(test1)
plot(test1)


plot(coldPoolShape, col="black")
boundaryPlot <- plot(boundaryShape, col="black")
graph_to_shp(graph = boundaryPlot, crds = pts_pop_simul, mode = "both",
             crds_crs = crds_crs,
             layer = "test_fonct",
             dir_path = tempdir(),
             metrics = FALSE)



coldPoolAndBoundary <- union(coldPoolShape, boundaryShape)
plot(coldPoolAndBoundary)

















coldPoolShape <- st_transform(coldPoolShape, crs = st_crs(rgb0))
boundaryShape <- st_transform(boundaryShape, crs = st_crs(rgb0))
plot(coldPool)
plot(boundaryShape)
plot(coldPool, add=TRUE)



crs.coldPoolShape <- projection(boundaryShape)
plot(boundaryShape)
plot(coldPoolShape, add=TRUE)
head(coldPoolShape)
head(boundaryShape)
identical(coldPoolShape,boundaryShape)

coldPoolShape$area <- st_area(st_geometry(coldPoolShape)) #calculate the area of the polgons and add a new attribute table called "area"
boundaryShape$area <- st_area(st_geometry(boundaryShape)) #calculate the area of the polgons and add a new attribute table called "area"
areascf <- as.data.frame(cbind(coldPoolShape$area,boundaryShape$area)) #bind the two columns together into a new dataframe. This will only work if both the original files have the same number of rows. if we are not certain that this is the case, we can check with length(st_geometry(new_SE)) for each file. 
#and inspect the new data frame we created:

head(areascf)

#rename the columns something more useful
colnames(areascf) <- c("coldPoolShape","boundaryShape")
#create a new column showing the difference in area between the two shape files
#I rounded up to 3 dps to avoid very small differences. 
areascf$diff <- round(areascf$coldPoolShape-areascf$boundaryShape,3)
areascf
plot(boundaryShape[1], col='red')
plot(coldPoolShape[1], col='blue', add=TRUE)
plot(areascf$diff)
