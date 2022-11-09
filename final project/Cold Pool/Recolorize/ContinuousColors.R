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
files<- list.files("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Cold Pool/Recolorize",pattern='*.nc',full.names=TRUE)
path <- "C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Cold Pool/Recolorize/"
dname <- "TEMP"

#extent
#Ext <- extent(c(259.3191, 455.6267, 137.5159, 354.8564))
Ext <- extent(c(222.6804, 584.4472, 109.7872, 349.5629))

# rasterVis plot
library(RColorBrewer)
greycols <- brewer.pal(9, 'Greys')
newcol <- colorRampPalette(greycols)
ncols <- 100
greycols2 <- newcol(ncols)#apply the function to get 100 colours
pie(rep(1, ncols), col = greycols2, border = NA, labels = NA)

mapTheme <- rasterTheme(region = rev(greycols2))
cutpts <- c(seq(-2, 16, by=0.25))

raster_list = list()
plot_list = list()
months_list = list( "-test1", "-test2", "-test3", "-test4")

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
rgb0 <- image_raster(minTemp, col = rev(greycols2))
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
