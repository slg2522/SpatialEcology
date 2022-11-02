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
mapTheme <- rasterTheme(region = rev(brewer.pal(5, "RdBu")))
cutpts <- c(-2,-1,0,1,2)

raster_list = list()
plot_list = list()
months_list = list( "-06", "-07", "-08", "-09")

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
  pltMin <- levelplot(minTemp, margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
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
















#check there are 5 rasters per folder
raster_list
testRaster <- raster(paste0(getwd(), "/", filetitle))
plot(testRaster, col=rainbow(100))
plot(minTemp)

#set plot color theme
# rasterVis plot
mapTheme <- rasterTheme(region = rev(brewer.pal(11, "RdBu")))
cutpts <- c(-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)


#view raster files

#raster5
raster5 <- raster(paste0(getwd(), "/", "R2200aRBRZBGCcaaa03a-05.tif"))
plot(raster5)
plt5 <- levelplot(raster5, margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                  main="1993-05")
#plot minimum temperature
plt5

#raster6
raster6 <- raster(paste0(getwd(), "/", "R2200aRBRZBGCcaaa03a-06.tif"))
plot(raster6)
plt6 <- levelplot(raster6, margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                  main="1993-06")
#plot minimum temperature
plt6

#raster7
raster7 <- raster(paste0(getwd(), "/", "R2200aRBRZBGCcaaa03a-07.tif"))
plot(raster7)
plt7 <- levelplot(raster7, margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                  main="1993-07")
#plot minimum temperature
plt7

#raster8
raster8 <- raster(paste0(getwd(), "/", "R2200aRBRZBGCcaaa03a-08.tif"))
plot(raster8)
plt8 <- levelplot(raster8, margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                  main="1993-08")
#plot minimum temperature
plt8

#raster9
raster9 <- raster(paste0(getwd(), "/", "R2200aRBRZBGCcaaa03a-09.tif"))
plot(raster9)
plt9 <- levelplot(raster9, margin = F,  at=cutpts, cuts=20, pretty=TRUE, par.settings = mapTheme,
                  main="1993-09")
#plot minimum temperature
plt9




## apply the colours used by raster plot() itself
raster5 <- image_raster(raster5, col = rev(brewer.pal(11, "RdBu")))
plotRGB(raster5)
raster6 <- image_raster(raster6, col = rev(brewer.pal(11, "RdBu")))
plotRGB(raster6)
raster7 <- image_raster(raster7, col = rev(brewer.pal(11, "RdBu")))
plotRGB(raster7)
raster8 <- image_raster(raster8, col = rev(brewer.pal(11, "RdBu")))
plotRGB(raster8)
raster9 <- image_raster(raster9, col = rev(brewer.pal(11, "RdBu")))
plotRGB(raster9)




plot(raster5)
plot(raster5)


library(palr)

## apply the colours used by raster plot() itself
rgb0 <- image_raster(minTemp, col = rev(brewer.pal(11, "RdBu")))
plotRGB(rgb5)

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

##############################################################
#the color functions
makePalette <- function(colourvector) {
  cmat = cbind(t(col2rgb(colourvector)), 255)
  res = apply(cmat, 1, function(x) {
    sprintf("<Entry c1=\"%s\" c2=\"%s\" c3=\"%s\" c4=\"%s\"/>", x[1], x[2], 
            x[3], x[4])
  })
  res = paste(res, collapse = "\n")
  res
}

makePaletteVRT <- function(raster, colourvector) {
  s = sprintf("<VRTDataset rasterXSize=\"%s\" rasterYSize=\"%s\">\n<VRTRasterBand dataType=\"Byte\" band=\"1\">\n<ColorInterp>Palette</ColorInterp>\n<ColorTable>\n", 
              ncol(raster), nrow(raster))
  p = makePalette(colourvector)
  s = paste0(s, p, "\n</ColorTable>\n</VRTRasterBand>\n</VRTDataset>\n")
  s
}

writePaletteVRT <- function(out, raster, colourvector) {
  s = makePaletteVRT(raster, colourvector)
  cat(s, file = out)
}

writePaletteVRT("test.vrt", testRaster, rainbow(20))
##############################################################







