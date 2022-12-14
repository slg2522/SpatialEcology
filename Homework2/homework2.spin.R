#HW 2 ~ Working with Spatial Data in R

#Due Wednesday, Sept. 14, 2022 via GitHub

#Assignment: HW#2 is designed to build your skills at working with,
#manipulating, and plotting spatial data in R. You are provided with a shapefile
#and must download a set of climate rasters and species occurrence data. You
#will then perform various tasks using these three datasets. You will be graded
#on your ability to produce clean, well commented R code that performs the tasks
#listed below. The hope is that I will be able to run your code without any
#errors. You can use R Markdown if you wish, but you are not required to do so.
#When you are done, push your code to GitHub, following the instructions
#provided in the document: mees698C.submittingHW.pdf.

#Keep in mind this is a HW assignment, not an exam, so please do not hesitate to
#ask questions if you get stuck. Some of these tasks are challenging! Refer to
#the advanced spatial data tutorial we started in class, which provides examples
#of most of the steps required to complete this assignment.

#Load necessary packages
library(rgdal) # 'Geospatial' Data Abstraction Library ('GDAL')
library(raster) # for all things raster and more
library(dismo) # species distribution modeling and much more
library(maps) # quick plotting of countries, etc.
library(gtools) # various functions
library(rasterVis) # raster visualization methods
library(fields) # Curve / function fitting for spatial analyses
library(tcltk) #build GUIs for R interface


#1. Use the raster::getData function to download the Worldclim climate data set
#at 2.5 arc-minute resolution. You want the bioclimatic variables (use
#var="bio", see ?getData in the raster package).

#download the world climate data at 2.5 arc-minute resolution (res) with the
#bio variables (var)
bioclimVars <- getData(name="worldclim", 
                       res = 2.5, # resolution
                       var = "bio") #variable



#2. Make a raster stack of the bio10, bio11, bio18, and bio19 bioclimatic
#variables only and clip this raster stack to the outline of Australia (not the
#extent) using the shapefile provided with this assignment.(Note that the
#shapefile also contains New Zealand, so you will have to do something about
#that before you perform the clipping operation, among other things. . . ).

#Make a raster stack of the bio10, bio11, bio18, and bio19 bioclimatic variables
#collect raster files from disk and read them as a stack:
file.remove(paste(getwd(), "/wc2-5/", "bio_2-5m_bil.zip", sep=""))
# sort the file names using ?mixedsort
files <- list.files(path=paste(getwd(), "/wc2-5/", sep=""), 
                    full.names=T, 
                    pattern=".bil")
#sort the file paths
list.ras <- mixedsort(files)
list.ras
#pull out 10, 11, 18, and 19 for the stack
bioclimStack <- stack(list.ras[10], list.ras[11], list.ras[18], list.ras[19])
bioclimStack

# Read shapefile
outline <- shapefile("Homework2/oz_nz_aea.shp")
class(outline)
plot(outline)
outline@proj4string

# Crop outline of Australia manually by drawing region of interest
plot(outline)
# click twice on the map to select the region of interest
drawExt <- drawExtent()    
drawExt
#class      : Extent 
#xmin       : -2249728 
#xmax       : 2050870 
#ymin       : -4972158 
#ymax       : -1089487 
#crop the outline
bioclimVars.sw <- crop(outline, drawExt)
#plot the new outline
plot(bioclimVars.sw)


# now crop the raster stack using the new outline
swOutline <- spTransform(bioclimVars.sw, projection(bioclimStack))
polyExt <- extent(swOutline)
bioclimVars.sw2 <- crop(bioclimStack, polyExt)
#plot the new region of interest and accompanying data
plot(bioclimVars.sw2)



#3. Use the dismo::gbif function to download records for the Austral grass tree
#(Xanthorrhoea australis).Clean up the resulting data frame by removing records
#without geographic coordinates & those that fall outside the Australian
#mainland. Convert the data to a SpatialPointsDataFrame with the correct CRS and
#containing only these attributes: acceptedScientificName, institutionCode, lon,
#lat, and year. Save your SpatialPointsDataFrame as a shapefile.

#load library
library('jsonlite')

#download records for the Austral grass tree (Xanthorrhoea australis)
#download up to 300 records in a single request (nrecs=300 max) and only
#retrieve records that have a georeference (geo=T) and fall within the specified
#extent (ext=drawExt)
xaustralis <- gbif("Xanthorrhoea", species="australis", ext=polyExt, nrecs=300,
                   geo=T)

#save the projection information stored in the 'geodeticDatum column
unique(xaustralis$geodeticDatum)
#correct CRS
crsxaustralis <- "WGS84"

#Convert the data to a SpatialPointsDataFrame and subset the columns to include:
#acceptedScientificName, institutionCode, lon, lat, year
xaustralis <- subset(xaustralis, select=c("acceptedScientificName", 
                                          "institutionCode", "lon", "lat", 
                                          "year"))

# review the attributes
dim(xaustralis)
#check for duplicates
duplicated(xaustralis) #lots of duplicated records - common problem with GBIF
#data
#keep only unique
xaustralis <- unique(xaustralis)
#view new dimensions
dim(xaustralis)

#check for NAs
which(is.na(xaustralis$lat)) #no NAs
which(is.na(xaustralis$lon)) #no NAs
#convert to spatial object
coordinates(xaustralis) <- c("lon", "lat")

#plot coordinates on world map in blue
map('world')
plot(xaustralis, pch=21, bg="blue", cex=1, add=T)

#assign CRS information  geographical, datum WGS84
crsxaustralis
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")    
# define projection system of our data
crs(xaustralis) <- crs.geo     

# polygon for Australia
plot(bioclimVars.sw)
#remove Tasmania
# click twice on the map to select the region of interest
drawExt2 <- drawExtent()    
drawExt2
#class      : Extent 
#xmin       : -2288918 
#xmax       : 2121845 
#ymin       : -4342060 
#ymax       : -1138015 
#crop the outline
australiaMainland <- crop(bioclimVars.sw, drawExt2)
#plot the new outline
plot(australiaMainland)
australiaMainland@proj4string

#use the 'over' function to select points that fall within the mainland of
#Australia project / transform the data first
swPolyProj2 <- spTransform(australiaMainland, CRSobj=projection(xaustralis))
xaustralis2 <- over(xaustralis, swPolyProj2)
# records outside of the polygon
which(is.na(xaustralis2))
#create new set with non mainland points removed
xaustralis3 <- xaustralis[-which(is.na(xaustralis2)),]
#plot only the mainland X. australis
plot(swPolyProj2)
plot(xaustralis3, pch=21, bg=rgb(0,0,1,0.5), add=T)

#Save an Australia Mainland shapefile to disk
outfile <- 'australiaMainland.shp'
shapefile(swPolyProj2, outfile, overwrite=FALSE)

#Save the xaustralis3 SpatialPointsDataFrame as a shapefile
class(xaustralis3)
outfile <- 'xaustralis3.shp'
shapefile(xaustralis3, outfile, overwrite=FALSE)



#4. Make a simple map of the cleaned species occurrence records from GBIF, using
#a color ramp or symbolization scheme to indicate the year the record was
#collected. Make sure to include the polygon of Australia and plot bio10 as the
#background. NOTE: All data in this map should be in the original projection of
#the Australia & New Zealand shapefile (i.e., not WGS84). Save the transformed
#bio10 raster as a GeoTiff.

#find number of years to color
xaustralis3
numyears <- 2022-1770

#rainbow palette for the scatter year points
my.palette <- rainbow(numyears)

#extract just bio10
bio10Background <- stack(list.ras[10])
# crop the bio10Background outline
background <- spTransform(australiaMainland, projection(bio10Background))
polyExt2 <- extent(background)
background.sw <- crop(bio10Background, polyExt2)

#plot the new region of interest and accompanying data
backPlot <- plot(background.sw)

#check the plot
backPlot

#align plots side-by-side
par(mfrow=c(1,2))
#arrange on top of one another
allPlot <- plot(background.sw) + plot(xaustralis3, pch=16, col=rainbow(numyears), add=T)

#years that the scalebar must show
years <- c(1770:2022)
#scalebar for scatter points
colorbar <- image(0, years, t(seq_along(1770:2022)), col=my.palette, axes=FALSE) + axis(4)

#save as a GeoTiff (.tif)
# Provide Raster* object and filename. The file format is assumed from 
# the filename extension. Or you can use 'format' argument.
# The 'datatype' argument can be used to set whether data are
# integer, float, etc.
saveRaster <- writeRaster(background.sw, 'bio10Raster.tif', overwrite=F)
saveRaster # file available



#5. Use the cleaned species occurrence data to extract the bioclimatic variables
#from the raster stack and compare the climate conditions where this species has
#been observed to the broader climate of Australia. A few hints: Have a look at
#the raster::sampleRandom function. To perform the comparison between climates
#where the species is present and Australia more broadly, you have a number of
#options. You might try scatter plots, box plots or histograms, but you do not
#need to do any statistical analyses (in other words, see what you can learn
#from simple plots alone - that???s enough for this assignment). Answer the
#question: How does the climate where X. australis has been observed differ from
#that of Australian climates more generally?


# crop the rasterstack outline to just mainland australia
mainlandStack <- spTransform(australiaMainland, projection(bioclimVars.sw2))
polyExt3 <- extent(mainlandStack)
mainlandStack.sw <- crop(bioclimVars.sw2, polyExt3)

#tree random sample
treeRandom <- sampleRandom(bioclimVars.sw2, 1000, na.rm=TRUE, ext=xaustralis3, 
                           cells=FALSE, rowcol=FALSE, xy=FALSE, sp=FALSE, asRaster=FALSE)

#australia random sample
australiaRandom <- sampleRandom(bioclimVars.sw2, 1000, na.rm=TRUE, ext=polyExt3, 
                                cells=FALSE, rowcol=FALSE, xy=FALSE, sp=FALSE, asRaster=FALSE)

#convert to dataframe
treeRandomDF <- as.data.frame(treeRandom)

#convert to dataframe
australiaRandomDF <- as.data.frame(australiaRandom)

#Analyses for the warmest Quarter
#find average temperature of each
averageTreeBio10 <- mean(treeRandomDF$bio10)
averageAustraliaBio10 <- mean(australiaRandomDF$bio10)
bio10Temp <- c(averageTreeBio10, averageAustraliaBio10)

#find average rainfall of each
averageTreeBio18 <- mean(treeRandomDF$bio18)
averageAustraliaBio18 <- mean(australiaRandomDF$bio18)
bio18Prec <- c(averageTreeBio18, averageAustraliaBio18)

par(mfrow=c(1,1))

#temperature bar chart showing averages for Bio10
barplot(bio10Temp,xlab="Regin",ylab="Temperature*10 in the warmest Quarter (C)",main="Bio10", names.arg=c('X. australis', "Australia"),col=c("green", "yellow"))

#precipitation bar chart showing averages for Bio18
barplot(bio18Prec,xlab="Region",ylab="Precipitation*10 in the warmest Quarter (in)",main="Bio18", names.arg=c('X. australis', "Australia"),col=c("green", "yellow"))

#set up warm quarter temperature and rainfall dataframes
TreeBio10 <- data.frame(temp=treeRandomDF$bio10, region='X. australis', color='green')
AustraliaBio10 <- data.frame(temp=australiaRandomDF$bio10, region='Australia', color='yellow')
TreeBio18 <- data.frame(precip=treeRandomDF$bio18, region='X. australis', color='green')
AustraliaBio18 <- data.frame(precip=australiaRandomDF$bio18, region='Australia', color='yellow')
warmQuarter <- data.frame(Temp=c(TreeBio10$temp, AustraliaBio10$temp), Precip=c(TreeBio18$precip, AustraliaBio18$precip), Region=c(TreeBio10$region, AustraliaBio10$region), Col=c(TreeBio10$color, AustraliaBio10$color))

#plot the temperature vs precipitation for Warmer Quarter
plot(warmQuarter$Temp, warmQuarter$Precip, main="Warm Quarter", xlab="Temperature*10 (C)", ylab="Precipitation*10 (in)", pch=16, col=warmQuarter$Col)
legend(x=150,y=1200,c("X. australis region","Australia mainland"),cex=.8,col=c("green","yellow"),pch=16)

#Bio10 temperature boxplot
boxplot(Temp~Region,data=warmQuarter, main="Temperature*10 (C) Bio10",
        xlab="Region", ylab="Temperature*10 (C)", col=c("green", "yellow"))

#Bio18 precipitation boxplot
boxplot(Precip~Region,data=warmQuarter, main="Precipitation*10 (in) Bio10",
        xlab="Region", ylab="Precipitation*10 (in)", col=c("green", "yellow"))



#Analyses for the Coolest Quarter
#find average temperature of each
averageTreeBio11 <- mean(treeRandomDF$bio11)
averageAustraliaBio11 <- mean(australiaRandomDF$bio11)
bio11Temp <- c(averageTreeBio11, averageAustraliaBio11)

#find average rainfall of each
averageTreeBio19 <- mean(treeRandomDF$bio19)
averageAustraliaBio19 <- mean(australiaRandomDF$bio19)
bio19Prec <- c(averageTreeBio19, averageAustraliaBio19)

#temperature bar chart showing averages for Bio10
barplot(bio11Temp,xlab="Region",ylab="Temperature*10 in the Coolest Quarter (C)",main="Bio11", names.arg=c('X. australis', "Australia"),col=c("green", "yellow"))

#precipitatin bar chart showing averages for Bio19
barplot(bio19Prec,xlab="Region",ylab="Precipitation*10 in the Coolest Quarter (in)",main="Bio19", names.arg=c('X. australis', "Australia"),col=c("green", "yellow"))

#set up cool quarter temperature and rainfall dataframes
TreeBio11 <- data.frame(temp=treeRandomDF$bio11, region='X. australis', color='green')
AustraliaBio11 <- data.frame(temp=australiaRandomDF$bio11, region='Australia', color='yellow')
TreeBio19 <- data.frame(precip=treeRandomDF$bio19, region='X. australis', color='green')
AustraliaBio19 <- data.frame(precip=australiaRandomDF$bio19, region='Australia', color='yellow')
coolQuarter <- data.frame(Temp=c(TreeBio11$temp, AustraliaBio11$temp), Precip=c(TreeBio19$precip, AustraliaBio19$precip), Region=c(TreeBio11$region, AustraliaBio11$region), Col=c(TreeBio11$color, AustraliaBio11$color))

#plot the temperature vs precipitation for cool Quarter
plot(coolQuarter$Temp, coolQuarter$Precip, main="Cool Quarter", xlab="Temperature*10 (c)", ylab="Precipitation*10 (in)", pch=16, col=coolQuarter$Col)
legend(x=10,y=600,c("X. australis region","Australia mainland"),cex=.8,col=c("green","yellow"),pch=16)

#Bio11 temperature boxplot
boxplot(Temp~Region,data=coolQuarter, main="Temperature*10 (c) Bio11",
        xlab="Region", ylab="Temperature*10 (C)", col=c("green", "yellow"))

#Bio19 precipitation boxplot
boxplot(Precip~Region,data=coolQuarter, main="Precipitation*10 (c) Bio19",
        xlab="Region", ylab="Precipitation*10 (in)", col=c("green", "yellow"))


#How does the climate where X. australis has been observed differ from
#that of Australian climates more generally?
#The mean temperature of the warmest quarter is slightly higher in Australia's
#mainland compared to X. australis' range (barchart bio10), however, they do not
#appear significantly different, disregarding outliers (boxplot bio10).
#The mean rainfall of the warmest quarter is substantially higher in Australia's
#mainland compared to X. australis' range (barchart bio18)however, they do not
#appear significantly different, disregarding outliers (boxplot bio18).
#X. australis appears to inhabit a slightly cooler and drier habitat in the warm
#quarter than the overall Australia mainland (scatterplot bio10 bio18).
#The mean temperature of the coolest quarter is slightly higher in Australia's
#mainland compared to X. australis' range (barchart bio11), however, the
#overlapping errorbars and large presence of outliers suggests no statistical
#difference.
#The mean rainfall of the coolest quarter is substantially higher in X.
#australis'region compared to the mainland (barchart bio19), however, the
#overlapping errorbars and large presence of outliers suggests no statistical
#difference.
#The X. australis region appears cooler and wetter than the mainland in the
#coolest quarter.




#6. Create a raster of the number of species observations in each grid cell. You
#might try using the rasterize function or perhaps by extracting the cell number
#for each observation and counting the number of times each cell number is
#duplicated (indicating the number of observations in that cell). This can be a
#tough one, so don???t hesitate to check in if you get stuck.

#each observation will be worth 1 count
xaustralis3$count <- 1

#rasterize using the spatial points data frame, the prior raster, the "count" column, and the function sum
sumTree <- rasterize(xaustralis3, background.sw, "count", fun='count')
#now raster class of the frequency of points per gridcell
class(sumTree)
#check the number of observations per gridcell
freq(sumTree)

library('sprawl')
#histogram of frequency by grid cell
plot_rasthist(sumTree, variable = "freq", type = "hist",
              verbose = TRUE)

#plot the number of observations
plot(sumTree)
# zoom in t see the counts by clicking two locations on the plot
#I zoomed in three times to see the locations with better detail
zoom(sumTree)
zoom(sumTree)
zoom(sumTree)

#Mirror to fitzLab-AL/mees698C-2022-118138886 created 9/13/2022 at 3pm.


