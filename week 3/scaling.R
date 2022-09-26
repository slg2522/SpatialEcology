# load libraries
library(gstat)
library(sp)
library(raster)
library(kriging)

# variogram: examine patterns --------------------------------------------------
# Meuse river data -- heavy metal concentrations in flood plain
data(meuse)
head(meuse)

# plot of data points, scaled to zinc concentration
# note how I scaled the size of the plotted points using cex
plot(meuse$x, meuse$y, 
     pch=20, # symbol type
     cex=meuse$zinc/100, # symbol size
     col=rgb(0,0,0,0.5)) # symbol color

# first, lets convert from a data frame to a spatial object, which is
# needed for variogram function 
coordinates(meuse) <- c("x", "y") # names of columns that contain coordinates

# variogram for zinc from "examples" in "gstat"
# the ~1 is a model that assumes stationarity (constant trend)
#calculates the dissimilarity and then fits a line
?variogram
vgm1 <- variogram(object=log(zinc)~1, data=meuse)
#np is the number of points, dist is distance, gamma is the autocorrelation
vgm1
# plot the observations
#range is about 1000 units
plot(vgm1, pch=20, col="blue", cex=2)

# next, we want to add an empirical model and plot the fitted line
# let's have a look at different model choices
# these are just examples of possible models and are NOT fitted 
# to the meuse data

#show variogram shows you examples of the types of variograms
show.vgms()

# spherical ("Sph") is a good choice
model.1 <- fit.variogram(vgm1, model=vgm("Sph"))
model.1
plot(vgm1, model=model.1) 

# fit more than one model
# the fit.variogram function will select the best fitting
vmod.best <- vgm(c("Sph", "Exp", "Gau", "Mat", "Wav"))
model.best <- fit.variogram(vgm1, vmod.best)
model.best #"Sph" is best fit

# ~1 assumes a constant trend, but a function can be used instead
# here we use sqrt(distance) as a predictor
vgm2 <- variogram(log(zinc)~sqrt(dist), meuse)
model.2 <- fit.variogram(vgm2, model=vgm("Sph"))
model.2
plot(vgm2, model=model.2)

#from documentation: "	
#direction in plane (x,y), in positive degrees clockwise from positive y (North): alpha=0 for direction North (increasing y), alpha=90 for direction East (increasing x); optional a vector of directions in (x,y)"

# Recall that we are assuming stationarity
# Lets check if the pattern is the same in all directions
# we use the alpha argument to set directions (degrees clockwise from North)
# variogram log(zinc) as a function of direction of transect
vgm2 <- variogram(log(zinc)~1, data=meuse, alpha=c(0,45,90,135))
plot(vgm2)

#stationarity assumes that one model is the same in all directions, but you could have different patterns in each direction
#in this model, the data are not stationarity in each direction

# variogram: predictions -------------------------------------------------------
# lets generate some data and examine the patterns
# Make a landscape
set.seed(614623) 

# Generate some random points with a fake 'habitat' variable
pts <- data.frame(cbind(xCoord=runif(100,0,100), yCoord=runif(100,0,100), 
                        habitat=runif(100,0,10))) 
head(pts)
plot(pts$xCoord, pts$yCoord, cex=pts$habitat)

# we will use the spatial structure in the data to interpolate between points.
# This process is known as 'kriging' - based on the semivarigram and a 
# spherical model (in this example)
# then interpolate between then using kriging to create an environmental surface
env <- kriging(x=pts$xCoord, y=pts$yCoord, response=pts$habitat,
               model="spherical") # see also ?krige

# Next, we can use the 'rasterFromXYZ' to turn the output into a raster
env <- rasterFromXYZ(env$map) #habitat raster
plot(env)
title("Interpolated raster")

# make pts a spatial object
ptsSp <- pts
coordinates(ptsSp) <- ~xCoord+yCoord #note different syntax, but same result
plot(ptsSp, cex=ptsSp$habitat/5, add=T)

# Now, let's fit a variogram to these random data
# What do we expect as the result?
#we expect a flat line because the adta are random and should therefore be no spatial structure (unautocorrelated all one line)
vgm1 <- variogram(habitat~1, data=ptsSp)
dev.off()
plot(vgm1)

# add fitted line
vmod <- vgm("Sph") # fit a variogram model
model.1 <- fit.variogram(vgm1, vmod)
plot(vgm1, model=model.1) # what is going on here? 

# lets generate a spatial pattern with structure
# in essence, I am adding a bit of error to the spatial coordinates
# which will result in a pattern with spatial structure
#creates 100 random points and then add a little bit of error
pts$habitat <- runif(100, -5, 5) + (abs(50-pts$xCoord)) + (abs(50-pts$yCoord))
plot(pts$xCoord, pts$yCoord, cex=pts$habitat/20)
coordinates(pts) <- ~xCoord+yCoord

# let's interpolate so we can make a raster / plot
env <- kriging(x=pts$xCoord, y=pts$yCoord, response=pts$habitat,
               model="spherical") # see also ?krige

# Next, we can use the 'rasterFromXYZ' to turn the output into a raster
env <- rasterFromXYZ(env$map) #habitat raster
plot(env)
title("Interpolated raster with spatial structure")
#strong spatial pattern called isotropic (same in every direction)

# variogram
vgm1 <- variogram(habitat~1, data=pts)
dev.off()
plot(vgm1)
#get a linear trend because constant trend in variogram going from the center to the edges

# add fitted line
show.vgms()
vmod.best <- vgm(c("Sph", "Exp", "Gau", "Bes", "Lin"))
model.1 <- fit.variogram(vgm1, vmod.best)
#4/5 of the models had trouble converging
model.1 #'Gau' is best
plot(vgm1, model=model.1) 


# lets have a look at real dataset: eastern hemlock abundance
hemlock <- read.csv("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/week 3/hemlock.csv")
hemlock <- data.frame(x=hemlock$x, y=hemlock$y, z=hemlock$abundance)
plot(hemlock$x, hemlock$y, pch=21, cex=hemlock$z/10, bg=rgb(0.2,1,0.5,0.75),
     col=rgb(0,0,0,0.75))

# make variogram
coordinates(hemlock)=~x+y
v.hem <- variogram(log(z)~1, data=hemlock)
plot(v.hem)

# add fitted line
vmod <- vgm("Sph")
model.hem <- fit.variogram(v.hem, vmod, fit.kappa=T)
model.hem
plot(v.hem, model=model.hem)

# fitted line of best model
vmod <- vgm(c("Sph", "Exp", "Mat")) 
model.hem <- fit.variogram(v.hem, vmod, fit.kappa=T)
model.hem
plot(v.hem, model=model.hem)


# Correlogram ------------------------------------------------------------------
library(spatial)

# fit trend surface
data(meuse)
head(meuse)
meuse <- meuse[,c("x", "y", "zinc")] # select columns

# the 'correlogram' function requires a surface for analysis, so we
# need to convert the meuse data to a surface
names(meuse) <- c("x", "y", "z")
# fit a trend surface using least-squares
meuse.kr <- surf.ls(3, meuse)
# correlogram
correlogram(meuse.kr, # the trend surface
            nint = 50) # number of bins used for lags
#high correlations in the closest bin and then scoops suggesting the autocorrelation increases at certain distances
?correlogram
#if you cannot use the raster, use image(matrix())


# autocorrelation --------------------------------------------------------------
data(sunspot.month) # Historical sunspots, class(sunspot.month) = "ts"
plot(sunspot.month)
plot(sunspot.month[1:(50*12)])
plot(sunspot.year[1:50])

# autocorrelation
acf(sunspot.month, lag.max = 250, type = "correlation", plot = TRUE)

