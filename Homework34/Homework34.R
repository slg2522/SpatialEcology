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
library(ncf)
library(gstat)
library(spatstat)

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
wrenSample.sw3



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

#using the provided example from above and the spatial points object
cor <- correlog(x=wrenPoints@coords[,1],
y=wrenPoints@coords[,2],
z=wrenPoints@data$carolinaWren,
increment = 75000,
resamp = 1000)
plot(cor)




#What is your interpretation of the resulting plot? In other words, what does it tell you about the spatial
#pattern in abundance of the Carolina wren? Do you think the correlogram would look different if we sampled
#random locations instead of using a regularly spaced grid? Keep in mind that we have VERY good data in
#this HW example, including a detailed raster map of range-wide abundance. Data this good are unusual (but
#becoming more common!) - so when answering these questions, try to think about what you could learn from
#just the correlogram if you had abundance data at a few dozen locations instead of these detailed data. This
#paper might be helpful to guide your interpretation:
#Brown, James H., David W. Mehlman, and George C. Stevens. “Spatial variation in abundance.” Ecology
#76.7 (1995): 2028-2043.

#Similar to Brown et al. 1995, this correlogram shows peaks at short distances
# and long distances. High spatial autocorrelation at short distances indicates
#that sites near one another in space, regardless of numeric distance, support
#more similar numbers of wrens than comparisons to wider spread out sites.
#Nearby areas with lots of individuals tend to sit ajacent to other sites with 
#high densities, and vice versa (low density sites near low density sites).
#High spatial autocorrelation at large lag distances suggests that sites at the
#furthest-most edges of the sample range showed similar densities. This may be 
#because sites along the peripheral of the wrens' geographic range are expected
#to support fewer wrens whereas those in the middle of the range tend to allow
#for higher carrying capacities (ie. the hotspots mentioned prior). From this 
#type of pattern, Brown et al. 1995 concludes that environmental variables that
# limit abundance are slow-changing and predictable over a given geographic and
# temporal range.

#Given that the spatial autocorrelation is an aspect of nearest neighbors,
#rather than a metric governed strictly by numerical proximity, I would guess 
#that the correlogram would look similar even if we sampled random locations
#instead of using a regularly spaced grid. At the edges of the wrens' range, we
#would still expect the environment to support lower densities than in the most
#ideal conditions likely in the central-most sites. A correlogram with less
#sites that still includes sampling at the center and edge-most areas should
#still capture the autocorrelation, albeit, in a less pronounced pattern. If you
#had abundance data at a few dozen locations, instead of these detailed data,
#you would likely still see high spatial autocorrelation at short distances in
#the center of the wrens' range, but the survey may miss the pronounced
#edge-of-range similarity if the scope of the study only examined sites well
#within the wrens' range (ie. not the extremes). If the small sample included
#the extremes, then both autocorrelation in short distance to centrally located
#neighbors and autocorrelation in widely spaced edge sites would be visible. If
#the study included only several sites that were widely spaced and lacked edges,
#the spatial autocorrelation may appear negligible.




#4. Next, use the variogram function in the gstat package to calculate and plot a sample variogram from
#the abundance data. Note that you are not required to fit a statistical model, but you can if you want.
#Two questions: (1) Is the abundance pattern isotropic (use variograms to support your answer)? (2) If
#you were to design a study to sample abundance of the Carolina wren, is there a distances at which
#you could space the sample sites to eliminate spatial structure in the observations (again, refer to your
#variogram(s) to support your answer)?

# create variogram with the assumption of stationarity (~1 = constant trend)
v1 <- variogram(object=carolinaWren~1, data=wrenPoints)
#view the variogram: np is the number of points, dist is distance, gamma is the autocorrelation
v1
# plot the observations
plot(v1, pch=20, col="blue", cex=2)

#show all of the types of variograms
show.vgms()

#list all of the possible models
vmod.best <- vgm(c("Nug", "Exp", "Sph", "Gau", "Exc", "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Per", "Wav", "Hol", "Log", "Pow", "Spl"))
# use fit.variogram function to select the best fitting
model.best <- fit.variogram(v1, vmod.best)
model.best #"Nug" and "Gau" are the best fits
#plot with the model
plot(v1, model=model.best)

#Is the abundance pattern isotropic (use variograms to support your answer)?

# Lets check if the pattern is the same in all directions
v2 <- variogram(carolinaWren~1, data=wrenPoints, alpha=c(0,45,90,135))
#we can see there is anisotropy
plot(v2)
#the model does not evenly fit each direction, suggesting we should analyse each
#direction differently
plot(v2, model=model.best)
#for the Carolina Wrens, the data are not stationary in each direction
#other directions should be tested likely with lower tolerance angles

#If you were to design a study to sample abundance of the Carolina wren, is
#there a distances at which you could space the sample sites to eliminate
#spatial structure in the observations?

#The sill of the best fit Nug model occurs at 1339.546 and for Gau at 6662.450.
#Unfortunately, anisotropy suggests that these sills would not be effective for
#the 135 and 0 degree angles. To determine direction-specific sills, the data
#should be examined with lower tolerance angles and variograms fit to each. The
#sill can then be used as a proxy for the minimum distance at which spatial
#structure between observations can be eliminated.




#PART 2 - HW4 ~ point-pattern Analysis:
#For this assignment, write R scripts to complete the following tasks and answer each question.

#1. Simulate three types of point-patterns: (1) Complete Spatial Randomness, (2) clustered, and (3)
#segregated. For the point-pattern with Complete Spatial Randomness (CSR): What is lambda?

#Complete Spatial Randomness (homogeneous Poisson process):
#if you want to replicate
set.seed(1) 
dims <- 100 # window size
pts <- 500 # number of points
#CSR represents lambda*area as the mean intensity of a poisson-distributed
#number of points over an area in two-dimensional space
lambda <- pts/dims/dims
lambda
#lambda represents the intensity of the process and for CSR is thus a constant
#of 0.5. 

# generate a point pattern with csr using the rpoispp function
#I use one simulation, note, nsim can be changed to your liking
csr <- rpoispp(lambda, 
               win=owin(xrange=c(0,dims), yrange=c(0,dims)),
               nsim=1)

# Let's look at the details of the csr object
summary(csr)
#for lambda
summary(csr)$intensity

#example: 100 simulations:
csrMoreSims <- rpoispp(lambda, 
                       win=owin(xrange=c(0,dims), yrange=c(0,dims)),
                       nsim=100)
summary(csrMoreSims)

# plot the csr point-pattern
plot(csr, pch=20, main="CSR")


#Clustered:

#Second-order structure, but not first order
set.seed(1)
# Thomas process with constant intensity
thom <- rThomas(kappa=30, scale=0.02, mu=10, win=c(0,1,0,1))
plot(thom, pch=20,  main="Clustered") #fairly clustered dataset with only second order structure


#Segregated:

#create empty matrices for the x and y coordinates
points.x=matrix(data=NA, nrow=1, ncol=1)
points.y=matrix(data=NA, nrow=1, ncol=1)

#fill in the x coordinates with numbers 1-7 repeating
for (x in 0:7){
  points.x[x]=1
  points.x[x+7]=2
  points.x[x+14]=3
  points.x[x+21]=4
  points.x[x+28]=5
  points.x[x+35]=6
  points.x[x+42]=7
}

#fill in the y coordinates with numbers 1-7 serially
for (y in 0:7){
  points.y[y]=y
}

#make the y coordinates repeat serially
points.y <- c(points.y, points.y, points.y, points.y, points.y, points.y, points.y)

#combine the coordinates
points <- data.frame(x=points.x, y=points.y)

#jitter the coordinates
jittered.x <- jitter(points.x)
jittered.y <- jitter(points.y)

#make the coordinates a dataframe
points <- data.frame(x=jittered.x, y=jittered.y)

#plot to make sure they look jittered
plot(points, pch=20)

#convert the dataframe to a spatial object with window 0:8 both
segregated.sp <- as.ppp(points, c(0,8,0,8))
#plot the new distribution
plot(segregated.sp, pch=20, main="Segregated")




#2. Examine and interpret each of your simulated patterns using the G-, K- and F-tests.

#combined response for questions 2-3 because the interpretation requires the
#plots and results of the G-, K-, and F-tests called for by #3.




#3. Plot: (i) your simulated point-patterns (be sure to add an appropriate title so I know which is which)
#and (ii) the results of the G-, K- and F-tests for each pattern. Provide a brief interpretation of the G-,
#K- and F-test results.

#CSR
#the points plot
par(mfrow=c(2,2))
plot(csr, pch=20, main="CSR")

# F = Empty space function = point-to-nearest-event distribution
Fest(csr, # point pattern
     correction = "none")

# F-test on pattern with csr (no distance-dependent interactions or spatial structure)
plot(Fest(csr, correction = "none"), main="F-test, csr")

# G = Nearest Neighbour Distance function = event-to-event distribution
Gest(csr, # point pattern
     correction = "none")
plot(Gest(csr, correction = "none"), main="G-test, csr")

# Ripley's K
Kest(csr, # point pattern
     correction = "none")
plot(Kest(csr, correction = "none"), main="K-test, csr")

#Interpretation:
#The complete spatial randomness (CSR) plots closely follow the Fpoison and
#Gpoisson lines, indicating that the simulated CSR graph displays F-values
#statistically similar to those we'd obtain if the null hypothesis of randomness
#were true. This similarity suggests that the distribution of distances r from
# the arbitrary test locations to the earest event patterns are the same for the
#simulation and for the poisson. The gtest plot shows that the nearest neighbors in the simulated set
#are not clustered or dispersed by comparison to a traditional poisson
#distribution. In the K-test, the simulated data was notably lower than the
#poisson at further distances, indicating the possibility for some dispersion at
#large distances. However, this is likely due to the low number of simulations 
#and we would expect the csr to better match the theoretical with more data.


#Clustered
#the points plot
par(mfrow=c(2,2))
plot(thom, pch=20, main="Clustered")

# F = Empty space function = point-to-nearest-event distribution
Fest(thom, # point pattern
     correction = "none")

# F-test on pattern with Clustered
plot(Fest(thom, correction = "none"), main="F-test, Clustered")

# G = Nearest Neighbour Distance function = event-to-event distribution
Gest(thom, # point pattern
     correction = "none")
plot(Gest(thom, correction = "none"), main="G-test, Clustered")

# Ripley's K
Kest(thom, # point pattern
     correction = "none")
plot(Kest(thom, correction = "none"), main="K-test, Clustered")

#Interpretation:
#The simulated data is much lower than the poisson distribution on the ftest
#plot, indicating that regardless of distance, there is clustering. The Gtest
# shows the simulation much higher than the poisson throughout the short and
#middle distances, indicating significant clustering at small and medium
# distances but not very large. The simulation above the poisson line for the 
#Ktest plot suggests that there is significant clustering at almost all
#distances except those very large (where the two lines intersect.) At the
#greatest of distances, the simulation below the poisson indicates minor
#dispersion (likely negligible).


#Segregated
#the points plot
par(mfrow=c(2,2))
plot(segregated.sp, pch=20, main="Segregated")

# F = Empty space function = point-to-nearest-event distribution
Fest(segregated.sp, # point pattern
     correction = "none")

# F-test on pattern with Segregated
plot(Fest(segregated.sp, correction = "none"), main="F-test, Segregated")

# G = Nearest Neighbour Distance function = event-to-event distribution
Gest(segregated.sp, # point pattern
     correction = "none")
plot(Gest(segregated.sp, correction = "none"), main="G-test, Segregated")

# Ripley's K
Kest(segregated.sp, # point pattern
     correction = "none")
plot(Kest(segregated.sp, correction = "none"), main="K-test, Segregated")

#Interpretation:
#Upon visual inspection, the points appear relatively evenly spaced with some 
#noise added from the jitter. According to the ftest, the simulated distribution
#is the same as the poisson at small spatial scales but much more dispersed at 
#middle and larger distances. The Gtest suggests some dispersion at small to 
#medium large distances, but poisson-following at the largest distances. The 
#ktest suggests dispersion at small distances, consistency with poisson at medium
#and dispersion at large distances. These tests appear to disagree with one
#another.




#Hints & Questions
#-See rThomas or rMatClust for functions to generate a clustered point-pattern.

#-There are different ways to produce a segregated point-pattern. One might be to create a grid of equally
#spaced points (50 or more) and then use the jitter function on the coordinates to add a bit of noise.
#See as.ppp to coerce the object to a ppp object as needed for the G-, K- and F-tests.