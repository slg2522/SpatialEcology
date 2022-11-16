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
library(sf)

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
intersectBB2 <- crop(publicLand,bb.2)
intersectBB3 <- crop(publicLand,bb.3)
intersectBB4 <- crop(publicLand,bb.4)
intersectBB5 <- crop(publicLand,bb.5)
intersectBB6 <- crop(publicLand,bb.6)

#plot the public land used by the panthers in land type color compared to the
#outline in black
plot(public)
plot(intersectBB1, add=TRUE)
plot(intersectBB2, add=TRUE, legend=FALSE)
plot(intersectBB3, add=TRUE, legend=FALSE)
plot(intersectBB4, add=TRUE, legend=FALSE)
plot(intersectBB5, add=TRUE, legend=FALSE)
plot(intersectBB6, add=TRUE, legend=FALSE)
#the Brownian Bridge KDE (95th percentile) proportional measures of use
head(kernel.bivar.95@polygons)

#----------------------------#
#contrast home range estimates
#----------------------------#

#home range area estimates
kernel.95 <- getverticeshr(kernel.href.bivar, percent=95)
bb.95 <- getverticeshr(bb.panther, percent=95)

#contrast area
mcp95$area
kernel.95$area
bb.95$area


#plot the predictions vs the tracks
par(mfrow=c(1,3))
#MCP
plot(public)
plot(intersectMCP95, add=TRUE)
#panther locations
points(panthers, col=panthers$CatID)

#KDE
plot(public)
plot(intersectKDE95, add=TRUE)
#panther locations
points(panthers, col=panthers$CatID)

#BB
plot(public)
plot(intersectBB1, add=TRUE)
plot(intersectBB2, add=TRUE, legend=FALSE)
plot(intersectBB3, add=TRUE, legend=FALSE)
plot(intersectBB4, add=TRUE, legend=FALSE)
plot(intersectBB5, add=TRUE, legend=FALSE)
plot(intersectBB6, add=TRUE, legend=FALSE)
#panther locations
points(panthers, col=panthers$CatID)

#calculate the difference in percent use
Public <- c(percentMCP$public)
MCP_Per <- c(100-(mcp95$area/percentMCP$public)*100)
KDE_Per <- c(100-(kernel.95$area/percentMCP$public)*100)
BB_Per <- c(100-(bb.95$area/percentMCP$public)*100)
percentUsed <- data.frame(Panther, MCP_Per, KDE_Per, BB_Per)

#find the intersection of the mcp95 and public
publicSF <- st_as_sf(public)
plot(publicSF[1])
mcp95SF <- st_as_sf(mcp95)
plot(mcp95SF[1])
intMCP <- sf::st_intersection(publicSF[1], mcp95SF[1])
plot(intMCP[1])

#get the areas of each
publicSF$area <- st_area(st_geometry(publicSF)) #calculate the area of the polgons and add a new attribute table called "area"
percentMCP <- as.data.frame(cbind(publicSF$area,mcp95$area)) #bind the two columns together into a new dataframe. This will only work if both the original files have the same number of rows. if we are not certain that this is the case, we can check with length(st_geometry(new_SE)) for each file. 
#and inspect the new data frame we created:

head(percentMCP)

#rename the columns something more useful
colnames(percentMCP) <- c("public","mcp95")
#create a new column showing the difference in area between the two shape files
#I rounded up to 3 dps to avoid very small differences. 
percentMCP$difference <- round(percentMCP$public-percentMCP$mcp95,3)
#what proportion of the home ranges of subadults is on public (protected) land?
percentMCP


#find the intersection of the kde95 and public
kde95SF <- st_as_sf(kernel.95)
plot(kde95SF[1])
intKDE <- sf::st_intersection(publicSF[1], kde95SF[1])
plot(intKDE[1])

#get the areas of each
percentKDE <- as.data.frame(cbind(publicSF$area,kernel.95$area)) #bind the two columns together into a new dataframe. This will only work if both the original files have the same number of rows. if we are not certain that this is the case, we can check with length(st_geometry(new_SE)) for each file. 
#and inspect the new data frame we created:

head(percentKDE)

#rename the columns something more useful
colnames(percentKDE) <- c("public","kde95")
#create a new column showing the difference in area between the two shape files
#I rounded up to 3 dps to avoid very small differences. 
percentKDE$difference <- round(percentKDE$public-percentKDE$kde95,3)
#what proportion of the home ranges of subadults is on public (protected) land?
percentKDE


#find the intersection of the bb95 and public
bb95SF <- st_as_sf(bb.95)
st_crs(bb95SF) <- projection(publicSF)
plot(bb95SF[1])
intBB <- sf::st_intersection(publicSF[1], bb95SF[1])
plot(intBB[1])

#get the areas of each
percentBB <- as.data.frame(cbind(publicSF$area,bb.95$area)) #bind the two columns together into a new dataframe. This will only work if both the original files have the same number of rows. if we are not certain that this is the case, we can check with length(st_geometry(new_SE)) for each file. 
#and inspect the new data frame we created:

head(percentBB)

#rename the columns something more useful
colnames(percentBB) <- c("public","bb95")
#create a new column showing the difference in area between the two shape files
#I rounded up to 3 dps to avoid very small differences. 
percentBB$difference <- round(percentBB$public-percentBB$bb95,3)
#what proportion of the home ranges of subadults is on public (protected) land?
percentBB

#my interpretation of the results, answering the question for managers
#Based on the combined visual evidence of the side-by-side maps of panther tracks
#and predicted homeranges combined with the table above reporting percent of public
#land occupied by each panther, and the intersections of the panther tracks with
#the public land, the MCP model appears to best reflect panther 147's sole presence
#in the public land and the other panther's absence. The MCP only overpredicts
#presence on one polygon, whereas the KDE and BB overpredict on multiple polygons
#yet report high percentages of nonresource use in the public land. Therefore,
#if managers, want the most conservative estimate of panther ranges, they should
#use the MCP model, if they want the most generous, use the BB model, and if they
#want an inbetween estimate for public land use, KDE provides this. Based on the
#track data alone, the MCP most closely matches the tracks. This could of course
#change with specified KDE and BB parameters. For instance, a sigma2 of 450 was
#chosen arbitrarily and likely deserves more attention based on the biology of 
#the species.

#chosen home range estimator approach from the three listed in question one
#Brownian Bridge Movement Model

#justification for method
#If I were a manager of an endangered and highly mobile and territorial species, I would want to overestimate, rather
#than risk underestimating, that species' home range. For this reason, the brownian bridge model 
#appears to be the most suitable for outlining potential habitats of the Florida panther and their 
#intersection with public areas. The BBMM assumes that location errors are within
#a bivariate normal distribution and that movement between successive locations is
#psuedo-random, in that it is conditioned on the starting and ending locations. I
#believe that these two assumptions are satisfied because the panthers' tracks
#do not appear to trend towards a central location or direction and error appears
#isometric over space.



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


#a description of and justification for the resource selection design approach and statistical test
#Of the selection ratios that we have discussed, the widesII appears the most
#appropriate for this task. As a design II data approach, the method assumes
#equal habitat availability for all organisms, but measures the habitat use for
#each individual. The method is also capable of testing for identical habitat use
#versus habitat selection. Wides uses a Chi-square for each animal to test 
#habitat selection.


#methods

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

#create some new layers that represent continuous covariates of key landcover types 
#for panthers by moving window analysis
#proportion of urban area in 5km radius
urban <- land_sub
values(urban) <- 0
urban[land_sub==8] <- 1

#natural area
natural <- land_sub
values(natural) <- 0
natural[land_sub!=8] <- 1

#5 km moving window to get neighborhood proportion
fw <- focalWeight(land_sub, 5000, 'circle')
natural.focal <- focal(natural, w=fw, fun="sum", na.rm=T)
urban.focal <- focal(urban, w=fw, fun="sum", na.rm=T)

#merge into a single raster stack
layers <- stack(land_sub, natural.focal, urban.focal)
names(layers) <- c("landcover", "natural", "urban")

#plot
plot(layers)

#design II availability: population availability
#get availability points
set.seed(8)
rand.II <- sampleRandom(layers, size=1000)
rand.II <- data.frame(rand.II)

#inspect
head(rand.II)
str(rand.II)

rand.II.land <- as.factor(rand.II$landcover)
table(rand.II.land)

#sum up counts of each landcover type
avail.II <- tapply(rand.II.land, rand.II.land, length)

#inspect
avail.II

#add land-cover names
names(avail.II) <- as.character(newclass.names[1:10,2])

#inspect
avail.II

#remove exotics, which were not observed in use sample
avail.II <- avail.II[c(-14)]

#design II selection ratios using widesII
sel.ratioII <- widesII(u=useCatID[,c(2:ncol(useCatID))], a=as.vector(avail.II), 
                       avknown=FALSE, alpha = 0.05)

#inspect
sel.ratioII
sel.ratioII$wi
sel.ratioII$se.wi

#plot
plot(sel.ratioII)

#the statistical results
#sel.ratioII$wi
#coastalwetland        Cropland       OpenWater       Grassland           Urban          Barren 
#3.30132700      2.98685783      3.57006248      1.45261471      0.88983700      1.62397579 
#Dryprairie Freshwatermarsh           Shrub    CypressSwamp 
#0.08500149      0.47365960      0.10359556      0.25835978 

#sel.ratioII$se.wi
#coastalwetland        Cropland       OpenWater       Grassland           Urban          Barren 
#0.90899386      0.70721382      1.14930546      0.39260054      0.30233322      0.76937733 
#Dryprairie Freshwatermarsh           Shrub    CypressSwamp 
#0.05355014      0.17044895      0.06139076      0.26026283 

#interpretation of the results, answering the question for managers
#The widesII reports a selection ratio of 0.89 for the urban environment. This 
#suggests that there is slightly more available urban environment than there is
#currently used by panthers. For reference, a ratio 0f 1.0 would mean there is an
#equal ratio of used urban habitat to what is used. The results suggest that
#compared to availability, panthers are disproportionately exhibiting movement 
#patterns around coastal wetlands (3.30), openwater (3.57), and cropland (2.99)
#and most avoiding dry prairies (0.09), shrubs (0.10), and cypress swamps (0.26).
#Urban environments appear to not be only slighly avoided even when accounting for the
#lower estimates of the confidence interval (ie. ~0.58 selection ratio). In short,
#managers should recognize that panthers only slightly avoid developed land areas
#and that this conclusion includes standard errors of 0.3, leading the possible
#selection ratio to be between 1.19 (signaling movement towards urban areas)
#and 0.59 (avoidance of urban areas). Thus, managers should recognize that the 
#0.89 selection ratio signals slight avoidance but that results should be interpreted
#cautiously given that the selection ratio's uncertainty could lead it to fall
#slightly skewed in either behavioral direction.






