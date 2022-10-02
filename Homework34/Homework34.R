#HW 3 & 4 ~ Autocorrelation and Understanding G, F, and K-tests
#Due Wednesday, October 5, 2022 at 11:00AM

#Assignment: This is a two-part HW assignment. The first part involves analyses of spatial structure in
#bird abundance data using variograms and correlograms (HW3). The second set of questions is related to
#analyses of three simulated point-pattern datasets (HW4). As in HW2, you will be graded on your answers
#and your ability to produce clean, well commented R code that performs the tasks listed below.

#PART 1 - HW3 ~ Variograms & Correlograms:
#With this assignment, you are provided with a raster map of estimated Carolina wren abundance in North
#America from the eBird database (carolinaWren.tif). Our goals are to (1) use correlograms to try to
#understand the spatial structure in these data and (2) variograms to inform how you might go about designing
#a field sampling study in the hopes of minimizing autocorrelation. Perform the following tasks and answer
#the associated questions.

#Load necessary packages
library(rgdal) # 'Geospatial' Data Abstraction Library ('GDAL')
library(raster) # for all things raster and more
library(dismo) # species distribution modeling and much more
library(maps) # quick plotting of countries, etc.
library(gtools) # various functions
library(rasterVis) # raster visualization methods
library(fields) # Curve / function fitting for spatial analyses
library(tcltk) #build GUIs for R interface

#1. Use the sampleRegular function in the raster package to generate a sample of Carolina wren abundance
#at about 300-500 locations. To avoid sampling outside of the primary range of the Carolina wren (i.e.,
#outside of where abundance is relatively high), limit the extent of your sampling to the region where
#abundance is greater than zero. See Figure 1. The drawExtent function might be useful to help you
#define the sampling extent.

# Download world shapefile and store in HW34 folder
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/Homework34/world_shape_file.zip")
# You now have it in your current working directory, have a look!

# Unzip this file
system("unzip C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/Homework34/world_shape_file.zip")


# Read in the TM_WORLD_BORDERS_SIMPL-0.3.shp file with rgdal
worldspdf <- readOGR( 
  dsn= paste0(getwd(),"/Homework34/world_shape_file/") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

#check the shapefile
#looks good
plot(worldspdf, col="white", bg="black", lwd=0.25, border=0 )


# Read shapefile
outline <- shapefile("Homework34/world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp")
class(outline)
plot(outline)
outline@proj4string

# Crop outline of USA manually by drawing region of interest
plot(outline)
# click twice on the map to select the region of interest
drawExt <- drawExtent()    
drawExt
#crop the outline
cropUSA.sw <- crop(outline, drawExt)
#plot the new outline
plot(cropUSA.sw)

#load raster data in an R object called 'wrenArea'
wrenArea <- raster(paste0(getwd(), "/Homework34/carolinaWren.tif"))
wrenArea




#To avoid sampling outside of the primary range of the Carolina wren (i.e.,
#outside of where abundance is relatively high), limit the extent of your sampling to the region where
#abundance is greater than zero. See Figure 1. The drawExtent function might be useful to help you
#define the sampling extent.

# now crop the raster using the new outline
wrenOutline <- spTransform(cropUSA.sw, projection(wrenArea))
polyExt <- extent(wrenOutline)
wrenUSA.sw <- crop(wrenArea, polyExt)
#plot the new region of interest and accompanying data
plot(wrenUSA.sw)

#check still raster layer, so operable with sampleRegular function
class(wrenUSA.sw)
?sampleRegular

#Use the sampleRegular function in the raster package to generate a sample of
#Carolina wren abundance at about 300-500 locations.
wrenSample <- sampleRegular(wrenUSA.sw, size=500, ext=wrenArea, asRaster=TRUE)
class(wrenSample)

#show approximately where the data are
spplot(wrenSample)
plot(wrenSample)

# Crop outline of wrenSample manually by drawing region of interest
plot(wrenSample)
# click twice on the map to select the region of interest
drawExt <- drawExtent()    
drawExt
#crop the outline
wrenSample.sw <- crop(wrenSample, drawExt)
#plot the new outline
plot(wrenSample.sw)




#Once you have generated the regular grid of samples, make a map that shows Carolina wren abundance
#and your sampling locations, but plot only those sampling locations that overlap the land surface, as
#shown below.

#fix the small extra areas
plot(wrenSample.sw)
# click twice on the map to select the region of interest
drawExt <- drawExtent()    
drawExt
#crop the outline
wrenSample.sw2 <- crop(wrenSample.sw, drawExt)
#plot the new outline
plot(wrenSample.sw2)

wrenSample.sw3 <- na.omit(wrenSample.sw2)
plot(wrenSample.sw3)
#delete any cell that is less than 1 (ie. those that round to 0 or less)
wrenSample.sw3[wrenSample.sw3<1] <- NA
plot(wrenSample.sw3)

#convert the raster to spatial points
wrenPoints <- rasterToPoints(wrenSample.sw3, spatial=TRUE)

par(mfrow=c(1,1))
plot(wrenUSA.sw)
plot(wrenPoints, pch=20, col="blue", add=TRUE)




#Next, we will produce and plot a correlogram using the regular grid of abundance samples. I have found
#the correlog function in the ncf package to be one of the more easy methods to produce and plot
#correlograms in R. The following example code worked well for me:
#library(ncf)
#cor <- correlog(x=samps2@coords[,1],
#y=samps2@coords[,2],
#z=samps2@data$carolinaWren,
#increment = 75000,
#resamp = 1000)
#plot(cor)




#What is your interpretation of the resulting plot? In other words, what does it tell you about the spatial
#pattern in abundance of the Carolina wren? Do you think the correlogram would look different if we sampled
#random locations instead of using a regularly spaced grid? Keep in mind that we have VERY good data in
#this HW example, including a detailed raster map of range-wide abundance. Data this good are unusual (but
#becoming more common!) - so when answering these questions, try to think about what you could learn from
#just the correlogram if you had abundance data at a few dozen locations instead of these detailed data. This
#paper might be helpful to guide your interpretation:

#Brown, James H., David W. Mehlman, and George C. Stevens. “Spatial variation in abundance.” Ecology
#76.7 (1995): 2028-2043.




#4. Next, use the variogram function in the gstat package to calculate and plot a sample variogram from
#the abundance data. Note that you are not required to fit a statistical model, but you can if you want.
#Two questions: (1) Is the abundance pattern isotropic (use variograms to support your answer)? (2) If
#you were to design a study to sample abundance of the Carolina wren, is there a distances at which
#you could space the sample sites to eliminate spatial structure in the observations (again, refer to your
#variogram(s) to support your answer)?




#PART 2 - HW4 ~ point-pattern Analysis:
#For this assignment, write R scripts to complete the following tasks and answer each question.

#1. Simulate three types of point-patterns: (1) Complete Spatial Randomness, (2) clustered, and (3)
#segregated. For the point-pattern with CSR: what is lambda?





#2. Examine and interpret each of your simulated patterns using the G-, K- and F-tests.




#3. Plot: (i) your simulated point-patterns (be sure to add an appropriate title so I know whcih is which)
#and (ii) the results of the G-, K- and F-tests for each pattern. Provide a brief interpretation of the G-,
#K- and F-test results.





#Hints & Questions
#-See rThomas or rMatClust for functions to generate a clustered point-pattern.

#-There are different ways to produce a segregated point-pattern. One might be to create a grid of equally
#spaced points (50 or more) and then use the jitter function on the coordinates to add a bit of noise.
#See as.ppp to coerce the object to a ppp object as needed for the G-, K- and F-tests.