##Lab 6: Home Range and Resource Selection
##Central question
##Endangered FL Panthers have large home ranges in areas where there is also considerable human 
##development. This movement potentially puts them at risk for human-wildlife interactions.
##Therefore, researchers have tracked several FL Panthers to determine their space use, the extent 
##to which they use and/or avoid developed areas, and how much of their home range is within 
##protected vs unprotected lands. You will be graded on your answers and your ability to produce 
##clean, well commented R code that performs the tasks listed below.
##Files: The data for this homework was all included in the class recitation (with the exception of 
##the protected areas file).
##Landcover raster: panther_landcover.grd
##Florida Panther tracking locations: panthers.shp
##Landcover reclassification scheme: resistance reclass.txt
##Shapefile of protected areas: panther_publicland.shp

#load packages
library(raster)           #for raster covariate data; version 2.6-7 used
library(reshape2)         #for re-formatting data; version 1.4.3 used
library(rgdal)            #for reading different types of GIS files; version 1.3-4 used
library(adehabitatLT)     #for trajectory information; version 0.3.23 used
library(adehabitatHR)     #Home range estimation; version 0.4.15 used
library(adehabitatHS)     #for selection ratios; version 0.3.13 used
library(survival)         #for conditional logit model; version 2.42-3 used

#set working directory where data were downloaded
setwd(choose.dir())



##1. Home Range
##Young (subadult) cats are known to move between protected and unprotected properties.
##Managers would like to know what proportion of the home ranges of subadults is on public 
##(protected) land. They would further like to know how much the method used to measure the 
##home range influence the estimate of the proportion of use on public land.
##Instructions: Use the landcover provided, the protected areas file, and the tracking data for the
##subadult cats (age class = SA (for subadult)) with the following home range estimators: MCP, 
##KDE (95th percentile), and Brownian Bridge KDE (95th percentile). (Hint: look at the help file 
##for “crop” in the raster library)
##In your response include: 
##1. one or more maps of young cat home ranges and protected areas
##2. the proportional measures of use
##3. your interpretation of the results, answering the question for managers.


#load in the landcover data
land <- raster("panther_landcover.grd")
#check projection
projection(land)
#label projection for later use
crs.land <- projection(land)


#load the protected areas file
public <- readOGR("panther_publicland.shp")
#check projection
projection(public)
#assign projection
projection(public) <- crs.land

#load the tracking data
panthers <- shapefile("panthers.shp")
#check projection
projection(panthers)
#no need to reproject because the land and panthers projections match

#View the locations of each cat
summary(panthers)
unique(panthers$CatID) #"100" "131" "137" "130" "143" "147"
panthers$CatID<-as.factor(panthers$CatID)
levels(panthers$CatID) 
head(panthers)

#specify the subadult cats (age class = SA (for subadult))
panthers <- subset(panthers, AgeClass=='SA')
head(panthers)

#plot subadults on the landcover and outline the public land in black
plot(land)
#public land outline
plot(public, add=TRUE)
#panther locations
points(panthers, col=panthers$CatID)

#create a mask raster to use for clipping
publicRast <- raster(public, res=res(land))
publicRast <- setValues(publicRast, 1)
publicRast <- mask(publicRast, public)

#crop the land raster to the subadult cats' range
#force origins to be the same
origin(publicRast) <- origin(land)
#crop by multiplying the mask and land together
publicLand <- land*publicRast

#visualize using a map of young cat home ranges and protected areas
#plot the new publicLand
plot(publicLand)
#plot the public outline to ensure fit
plot(public, add=TRUE)
#plot the panther locations
points(panthers, col=panthers$CatID)

###################################################
#MCP home range analysis
###################################################

#compute the MCP home range estimator
mcp95 <- mcp(panthers[,"CatID"], percent = 95, unin = c("m"), unout = c("m2"))
mcp50 <- mcp(panthers[,"CatID"], percent = 50, unin = c("m"), unout = c("m2"))

#plot
dev.off()
plot(publicLand, axes=F, ylab="",xlab="", legend=TRUE)
plot(panthers, add=TRUE, col=panthers$CatID, pch=1)
plot(mcp95, add=TRUE)
plot(mcp50, add=TRUE, border="Grey")

#find the proportion of homerange intersecting with public lands
intersectMCP95 <- crop(publicLand,mcp95)
#plot the public land used by the panthers in color comapred to the outline in black
plot(public)
plot(intersectMCP95, add=TRUE)
#the MCP proportional measures of use and coordinates for mcp95
head(mcp95@polygons)

###################################################
#KDE home range analysis
###################################################

#compute the KDE (95th percentile) home range estimator

#kernel types:
kernel.href.bivar <- kernelUD(panthers[,"CatID"], h="href", kern="bivnorm")
kernel.href.epa <- kernelUD(panthers[,"CatID"], h="href", kern="epa")

#plot
image(kernel.href.bivar)
image(kernel.href.epa)

#alternative plots for each of the cats
plot(kernel.href.bivar[[1]])
plot(kernel.href.epa[[1]])
plot(kernel.href.bivar[[2]])
plot(kernel.href.epa[[2]])
plot(kernel.href.bivar[[3]])
plot(kernel.href.epa[[3]])
plot(kernel.href.bivar[[4]])
plot(kernel.href.epa[[4]])
plot(kernel.href.bivar[[5]])
plot(kernel.href.epa[[5]])
plot(kernel.href.bivar[[6]])
plot(kernel.href.epa[[6]])

#UD data for each cat
kernel.href.bivar[[1]]@data
kernel.href.bivar[[2]]@data
kernel.href.bivar[[3]]@data
kernel.href.bivar[[4]]@data
kernel.href.bivar[[5]]@data
kernel.href.bivar[[6]]@data

#h value for bandwidth
kernel.href.bivar[[2]]@h
kernel.href.bivar[[2]]@h$h

#least-squares cross validation for h
kernel.lscv.bivar <- kernelUD(panthers[,"CatID"], h="LSCV", kern="bivnorm")

#contour maps of activity for each cat
plot(kernel.href.bivar[[1]])
contour.data <- as.image.SpatialGridDataFrame(kernel.href.bivar[[1]])
contour(contour.data, nlevels=5, add=TRUE)
plot(kernel.href.bivar[[2]])
contour.data <- as.image.SpatialGridDataFrame(kernel.href.bivar[[2]])
contour(contour.data, nlevels=5, add=TRUE)
plot(kernel.href.bivar[[3]])
contour.data <- as.image.SpatialGridDataFrame(kernel.href.bivar[[3]])
contour(contour.data, nlevels=5, add=TRUE)
plot(kernel.href.bivar[[4]])
contour.data <- as.image.SpatialGridDataFrame(kernel.href.bivar[[4]])
contour(contour.data, nlevels=5, add=TRUE)
plot(kernel.href.bivar[[5]])
contour.data <- as.image.SpatialGridDataFrame(kernel.href.bivar[[5]])
contour(contour.data, nlevels=5, add=TRUE)
plot(kernel.href.bivar[[6]])
contour.data <- as.image.SpatialGridDataFrame(kernel.href.bivar[[6]])
contour(contour.data, nlevels=5, add=TRUE)

#comparing bivar and epa
kernel.bivar.95 <- getverticeshr(kernel.href.bivar, percent=95)
kernel.epa.95 <- getverticeshr(kernel.href.epa, percent=95)

#plot Comparison of Bivariate Normal 95% and Epanechnikov 95%
my_window <- extent(609162,722055,204743,393259)

par(mfrow=c(1,2), mar=c(0.2,0.2,3,0.2))  #sets up the graph window to store two graphs
plot(my_window, axes=F, main ="Bivariate Normal \n95% Kernel", 
     col=NA,ylab="",xlab="")
plot(publicLand, add=TRUE,legend=F) 
plot(kernel.bivar.95, add=TRUE, col=kernel.bivar.95$id)
plot(panthers, add=TRUE, pch=21, col="white") #col=panthers$CatID, 

plot(my_window, axes=F, main ="Epanechnikov \n95% Kernel", col=NA,ylab="",xlab="")
plot(publicLand, add=TRUE,legend=F)
plot(kernel.epa.95, add=TRUE, col=kernel.epa.95$id)
plot(panthers, add=TRUE, pch=21, col="white")

#find the proportion of homerange intersecting with public lands
intersectKDE95 <- crop(publicLand,kernel.epa.95)
#plot the public land used by the panthers in color compared to the outline in black
plot(public)
plot(intersectKDE95, add=TRUE)
#the KDE (95th percentile) proportional measures of use
head(kernel.epa.95@polygons)

#find the proportion of homerange intersecting with public lands
intersectKDEBN95 <- crop(publicLand,kernel.bivar.95)
#plot the public land used by the panthers in color compared to the outline in black
plot(public)
plot(intersectKDEBN95, add=TRUE)
#the KDE (95th percentile) proportional measures of use
head(kernel.bivar.95@polygons)

dev.off()

###################################################
#Brownian Bridge KDE home range analysis
###################################################

#computing the Brownian Bridge KDE (95th percentile) home range estimator
#requires the trajectory of each individual
#Re-format Juldate information
#function for taking characters of a string from rightmost value
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
#tracking trajectory data with real-time information

#re-format date
panthers$Juldate <- as.character(panthers$Juldate)
panther.date <- as.numeric(substrRight(panthers$Juldate, 3))
panthers$Date<-as.Date(panther.date, origin=as.Date("2006-01-01"))

## S3 method for class 'character'
#convert to POSIXct object
panthers$Date <- as.POSIXct(panthers$Date,"%Y-%m-%d", tz = "EST")

#convert to data frame
panther.df <- as.data.frame(panthers)

#Convert to trajectory object
panther.ltraj <- as.ltraj(xy=coordinates(panthers), date=panthers$Date, id=panthers$CatID, typeII=T)
summary(panther.ltraj)

#plot
plot(panther.ltraj)

#specify two parameters to fit a Brownian bridge: sigma1 and sigma2

#telemetry error (average)
#related to h in KDE, higher value more smoothing)
sigma2 <- 450

#estimate sigma1
#related to the speed of the animal, higher value is higher tortuosity
#maximum liklehood to estimate
liker(panther.ltraj, sig2 = sigma2, rangesig1 = c(2, 10))
liker(panther.ltraj, sig2 = sigma2, rangesig1 = c(2, 30))
sigma1 <- liker(panther.ltraj, sig2 = sigma2, rangesig1 = c(2, 100))

#inspect maximization of the log-likelihood parameter value for each animal
sigma1

#they are different enough for each cat that we should use 
#different values for each
#brownian bridge for Cat 147
bb.147 <- kernelbb(panther.ltraj[6], sig1 = 7.2, sig2 = sigma2, grid = 200)

#all panthers
sig1 <- c(sigma1[[1]]$sig1, sigma1[[2]]$sig1, sigma1[[3]]$sig1, sigma1[[4]]$sig1, sigma1[[5]]$sig1, sigma1[[6]]$sig1)
bb.panther <- kernelbb(panther.ltraj, sig1 = sig1, sig2 = sigma2, grid = 200)
bb.1 <- kernelbb(panther.ltraj[1], sig1 = sig1[1], sig2 = sigma2, grid = 200)
bb.2 <- kernelbb(panther.ltraj[2], sig1 = sig1[2], sig2 = sigma2, grid = 200)
bb.3 <- kernelbb(panther.ltraj[3], sig1 = sig1[3], sig2 = sigma2, grid = 200)
bb.4 <- kernelbb(panther.ltraj[4], sig1 = sig1[4], sig2 = sigma2, grid = 200)
bb.5 <- kernelbb(panther.ltraj[5], sig1 = sig1[5], sig2 = sigma2, grid = 200)
bb.6 <- kernelbb(panther.ltraj[6], sig1 = sig1[6], sig2 = sigma2, grid = 200)
#grid refers to the size of the grid to be estimated

#plot








#find the proportion of homerange intersecting with public lands
intersectBB1 <- crop(publicLand,bb.1)
#plot the public land used by the panthers in color compared to the outline in black
plot(public)
plot(intersectBB1, add=TRUE)
#the KDE (95th percentile) proportional measures of use
head(kernel.bivar.95@polygons)


bb.panther

#----------------------------#
#contrast estimates
#----------------------------#

#home range area estimates
kernel.95 <- getverticeshr(kernel.href.bivar, percent=95)
bb.95 <- getverticeshr(bb.panther, percent=95)

#contrast area
mcp95$area
kernel.95$area
bb.95$area










#the Brownian Bridge KDE (95th percentile) proportional measures of use




#what proportion of the home ranges of subadults is on public (protected) land


#how much the method used to measure the home range influences the estimate of
#the proportion of use on public land


#my interpretation of the results, answering the question for managers






##2. Resource Use
##Managers would also like to know if young Panthers, in particular, are avoiding developed land 
##cover types. 
##Instructions: Use the landcover provided and the tracking data for subadult cats (age class is SA 
##for subadult). The selection method that you use is up to you. Rather than comparing the results 
##from different methods (as above), just use one approach and describe your approach with a 
##justification of why you think this is the best approach to address the question.
##Hint: for urban, land_sub==8
##In your response include:
##1. a description of and justification for the resource selection design approach and statistical test
##used to answer the question
##2. the statistical results 
##3. your interpretation of the results, answering the question for managers.


#chosen home range estimator approach from the three listed in question one

#justification for selection to address the developed area avoidance question


#specify the urban environment using land_sub==8

# reclassify into fewer landcover categories
#load reclassification table for reclassifying map
classification <- read.table("resistance reclass.txt", header=T)
head(classification)
classification$Description<-as.factor(classification$Description)
classification$Description2<-as.factor(classification$Description2)

levels(classification$Description)    #original classification
levels(classification$Description2)   #re-class

#format for reclassify function;
class <- as.matrix(classification[,c(1,3)])
land_sub <- reclassify(land,rcl=class)
#plot only the urban areas
plot(land_sub==8)


#the statistical test to answer the question

#the statistical test results

#my interpretation of the statistical test results and answer to managers


