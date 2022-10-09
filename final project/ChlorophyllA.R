
#Load necessary packages
library(rgdal) # 'Geospatial' Data Abstraction Library ('GDAL')
library(raster) # for all things raster and more
library(dismo) # species distribution modeling and much more
library(maps) # quick plotting of countries, etc.
library(gtools) # various functions
library(rasterVis) # raster visualization methods
library(fields) # Curve / function fitting for spatial analyses
library(tcltk) #build GUIs for R interface
library(ncf)
library(gstat)
library(spatstat)
library(sna)
library(ggplot2)

setwd("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/")

###############################################################################
#file handling

# Read region1 shapefile
region1 <- shapefile("region1/region1-polygon.shp")
class(region1)
plot(region1)
outline@proj4string

# Read region2 shapefile
region2 <- shapefile("region2/region2-polygon.shp")
class(region2)
plot(region2)
outline@proj4string

# Read region3 shapefile
region3 <- shapefile("region3/region3-polygon.shp")
class(region3)
plot(region3)
outline@proj4string

chl <- raster("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/MY1DMM_CHLORA_2002-07-04_rgb_3600x1800.TIFF")
chl
plot(chl)

################################################################################
#Raster Crops

# now crop the raster using the new outline
region1Outline <- spTransform(region1, projection(chl))
polyExt <- extent(region1Outline)
region1.sw <- crop(chl, polyExt)
#plot the new region of interest and accompanying data
plot(region1.sw)

#check still raster layer, so operable with sampleRegular function
class(region1.sw)

## crop and mask
region1Crop <- crop(region1.sw, extent(region1))
region1Mask <- mask(region1Crop, region1)

theme_set(theme_bw())

gplot(region1Mask) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradient(low = '#091E5A',
                      high = '#1ea84c',
                      na.value="white") +
  labs(x="longitude", y="latitude", title="Chlorophyll A Concentration Region 1") +
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.background = element_blank())
#gplot(region1, lwd=8, add=TRUE)


#Region 2
# now crop the raster using the new outline
region2Outline <- spTransform(region2, projection(chl))
polyExt <- extent(region2Outline)
region2.sw <- crop(chl, polyExt)
#plot the new region of interest and accompanying data
plot(region2.sw)

#check still raster layer, so operable with sampleRegular function
class(region2.sw)

## crop and mask
region2Crop <- crop(region2.sw, extent(region2))
region2Mask <- mask(region2Crop, region2)

theme_set(theme_bw())

gplot(region2Mask) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradient(low = '#091E5A',
                      high = '#1ea84c',
                      na.value="white") +
  labs(x="longitude", y="latitude", title="Chlorophyll A Concentration Region 2") +
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.background = element_blank())


#Region 3
# now crop the raster using the new outline
region3Outline <- spTransform(region3, projection(chl))
polyExt <- extent(region3Outline)
region3.sw <- crop(chl, polyExt)
#plot the new region of interest and accompanying data
plot(region3.sw)

#check still raster layer, so operable with sampleRegular function
class(region3.sw)

## crop and mask
region3Crop <- crop(region3.sw, extent(region3))
region3Mask <- mask(region3Crop, region3)

theme_set(theme_bw())

gplot(region3Mask) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradient(low = '#091E5A',
                      high = '#1ea84c',
                      na.value="white") +
  labs(x="longitude", y="latitude", title="Chlorophyll A Concentration Region 3") +
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.background = element_blank())

















#create for loop to speed this up


