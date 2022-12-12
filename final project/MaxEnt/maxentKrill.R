# load the required libraries
library(raster)
library(sp)
library(rgdal)
library(dismo)
library(sp)
library(sm)
library(colorRamps)
library(ENMeval)
library(terra)

################################################################################
# CHUNK 1: Read in & prep data
################################################################################

# set working directory to data folder
#setwd("pathToDirHere")
wd <- ("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/MaxEnt")
setwd(wd)

#load the rasters and create the necessary raster stacks prior to averaging

#chlorophyllA
#load in all files
ca <- list.files(path="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/MaxEnt/data/ChlorophyllA", pattern = "TIFF", full.names = TRUE)
#stack the files
stackCA <- raster::stack(ca)
#write a combined raster file
writeRaster(stackCA, "chlorophyllCombined.TIF")
#take the average
ca_avg <- mean(stackCA)
#plot the average
plot(ca_avg)


#sea surface temperature
#load in all files
sst <- list.files(path="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/MaxEnt/data/SeaSurfaceTemperature", pattern = "TIFF", full.names = TRUE)
#stack the files
stackSST <- raster::stack(sst)
#write a combined raster file
writeRaster(stackSST, "sstCombined.TIF")
#take the average
sst_avg <- mean(stackSST)
#plot the average
plot(sst_avg)
#terra does not take non-integer values, to convert, must take factors of 5 and 2
#change to 0.5 resolution
sst5 <- aggregate(sst_avg, 2)
plot(sst5)
#change to 0.1 resolution
sst1 <- disaggregate(sst5, 5)
plot(sst1)


#bathymetry
#load in all files
bathymetry <- list.files(path="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/MaxEnt/data/Bathymetry", pattern = "TIFF", full.names = TRUE)
#stack the files
stackBath <- raster::stack(bathymetry)
#write a combined raster file
writeRaster(stackBath, "bathymetryCombined.TIF")
#take the average
bath_avg <- mean(stackBath)
#plot the average
plot(bath_avg)

# thinned krill occurrence data for the Bering Sea
krill9500 <- read.csv("thin9500LonLat.csv")
krill0106 <- read.csv("thin0106LonLat.csv")
krill0712 <- read.csv("thin0712LonLat.csv")
krill1318 <- read.csv("thin1318LonLat.csv")

#check the data with headers
head(krill9500)
head(krill0106)
head(krill0712)
head(krill1318)

#combine all of the thinned krill data
krillGeoXY <- data.frame(species=c(krill9500$species, krill0106$species, krill0712$species, krill1318$species),
                         lon=c(krill9500$lon, krill0106$lon, krill0712$lon, krill1318$lon),
                         lat=c(krill9500$lat, krill0106$lat, krill0712$lat, krill1318$lat))

#crop rasters:
e <- extent(-180, -150, 50, 70)
#chlorophyllA
cropCA <- crop(ca_avg, e)
plot(cropCA)
#SST
cropSST <- crop(sst1, e)
plot(cropSST)
#Bathymetry
cropBATH <- crop(bath_avg, e)
plot(cropBATH)

#stack all of the environmental covariates
envCovariates <- stack(cropCA, cropSST, cropBATH)

# read in climate & elev data
neClim <- stack(envCovariates)
krillSp <- krillGeoXY # make a new object for use later
# convert to sp object
coordinates(krillSp) <- c("lon", "lat")
# assign projection
projection(krillSp) <- projection(neClim)

# let's look at the data
plot(neClim$layer.1, col=rgb.tables(1000))
points(krillSp, pch=20, cex=0.7, col="black")

# make a mask raster to use below
mask <- neClim[[1]]>-1000


# Sampling bias ----------------------------------------------------------------
# To deal with spatial sampling bias, we will create
# a raster that reflects the sampling density of krill
# in the study region using kernel density estimation (KDE)

# use the x-y coords to get the cell numbers
bias <- cellFromXY(mask, krillSp) # cells with records
cells <- unique(sort(bias))
# xy-locations of all samples
kernelXY <- xyFromCell(mask, cells)
samps <- as.numeric(table(bias)) 
# number of samples in each grid cell
head(samps)
max(samps) #one grid cell has 255 records in it

# code to make KDE (kernel density estimate) raster
# Our goal is to extrapolate sampling density across the study region
# and to make a raster that matches the env predictors exactly. To do that,
# we need to set some parameters to produce a grid that is close in size
# and resolution to the env rasters. The 'sm.density' function will perform
# 2D density estimation.
KDEsur <- sm.density(kernelXY, 
                     weights=samps, 
                     display="none", # do not plot 
                     ngrid=300, # number of grid cells along each dimension,
                     # there are 300 columns
                     ylim=c(50,70), # approximate latitude range 
                     xlim=c(-180,-150), # approximate longitude range
                     nbins=0) # see help, can ignore

# let's look at the structure
str(KDEsur)
head(KDEsur$eval.points) # point locations where denisty was estimated
KDEsur$estimate[1:5, 1:5] # the density estimates

# now we need to turn this output into a raster
# expand grid will make all possible combinations of two vectors
# in this case we want to get all possible combinates of the x-y coordinates
# which will produce a full grid
pointGrid <- expand.grid(x=KDEsur$eval.points[,1], y=KDEsur$eval.points[,2])
KDErast <- SpatialPoints(pointGrid) # convert to sp object
# And now a step closer to a grid
KDErast <- SpatialPixelsDataFrame(KDErast, # points
                                  data.frame(kde = array(KDEsur$estimate, # data
                                                         length(KDEsur$estimate))))
# convert to a raster and plot
KDErast <- raster(KDErast)
plot(KDErast)

# now crop, etc so that it matches the climate data
KDErast <- resample(KDErast, mask)
KDErast <- KDErast*mask
plot(KDErast)
stack(neClim, KDErast) # no errors!

# convert to points for use as background locations 
KDEpts <- rasterToPoints(KDErast)
bg <- randomPoints(KDErast, 500, prob=T) # alternate method
################################################################################


################################################################################
# CHUNK 2: Run models
################################################################################
# extract data for krill spp
krillSpp <- c("krill")
a <- 1 #only working with krill as a taxonomic group
# loop to run models for each species if there were multiple species of krill
#for(a in 1:length(krillSpp)){
krillGeoXY <- krillGeoXY[krillGeoXY$species==krillSpp[a],-1]
#check the dimensions
dim(krillGeoXY)
#plot
plot(neClim[[1]])
points(krillGeoXY$lon, krillGeoXY$lat, pch=20, col="black")

########## make sets of training/test (t/t) data
ttSplit = 0.25 # ttSplit = test/train split percentage
# function to partition data into t/t
fold <- function(ttSplit){ 
  k = round(1/ttSplit, 0)
  fold <- kfold(krillGeoXY, k=k)
}

# make sets of t/t data
sets = 5 # number of t/t sets to make. We used 50 in the paper.
folds <- replicate(sets, fold(ttSplit)) # replicate five different random folds
head(folds)

# now loop through to build lists of t/t data
krillTrain <- list()
krillTest <- list()
for(h in 1:sets){
  krillTrain[[h]] <- krillGeoXY[folds[,h]!=1,]
  krillTest[[h]] <- krillGeoXY[folds[,h]==1,]
}
str(krillTrain)

plot(neClim[[1]])
points(krillTrain[[1]], pch=20)
points(krillTrain[[2]], pch=20, col="red")



# now we will work through four different maxent models:
# (1) linear features only
# (2) Default features (set by number of occurrence records)
# (3) linear features only, bias corrected background
# (4) default features, bias corrected background



# MODEL 1 -----------------------------------------------------------------
#### MAXENT - LINEAR FEATURES
krillmaxMods_LF <- list()
# loop through each fold (5 total), fit a model, and place fitted model
# in a list
for(f in 1:sets){
  print(f)
  #### MAXENT - LINEAR FEATURES
  krillmaxMods_LF[[f]] <- maxent(neClim, krillTrain[[f]], 
                               args=c(c("-h", # turn off hinge features
                                        "-q", # turn off quadratic features
                                        "-p", # turn off product features
                                        "-P", # return response curves
                                        "nothreshold"), # turn off threshold
                                      c("-m", 10000))) # max iterations = 10K
}
# predict to geography
krillmaxStack_LF <- predict(krillmaxMods_LF[[1]], neClim) #cloglog
krillmaxStackRAW_LF <- predict(krillmaxMods_LF[[1]], neClim, args='outputformat=raw')

# loop through the rest of the models and predict
for(j in 2:sets){
  print(j)
  mod_LF <- predict(krillmaxMods_LF[[j]], neClim)
  modRAW_LF <- predict(krillmaxMods_LF[[j]], neClim, args='outputformat=raw')
  krillmaxStack_LF <- stack(krillmaxStack_LF, mod_LF)
  krillmaxStackRAW_LF <- stack(krillmaxStackRAW_LF, modRAW_LF)
}

plot(krillmaxStack_LF, col=rgb.tables(1000))
plot(mean(krillmaxStack_LF), col=rgb.tables(1000)) # mean of the five models
plot(calc(krillmaxStack_LF, sd), col=rgb.tables(1000)) #std dev of the five models







# MODEL 2 -----------------------------------------------------------------
#### MAXENT - DEFAULT FEATURES
# Same as above, but now using default features
krillmaxMods_allF <- list()
for(f in 1:sets){
  print(f)
  krillmaxMods_allF[[f]] <- maxent(neClim, krillTrain[[f]], 
                                 args=c("-P",c("-m", 10000)))
}
# Predict to geography and stack
krillmaxStack_allF <- predict(krillmaxMods_allF[[1]], neClim)
krillmaxStackRAW_allF <- predict(krillmaxMods_allF[[1]], neClim, args='outputformat=raw')
for(j in 2:sets){
  print(j)
  mod_allF <- predict(krillmaxMods_allF[[j]], neClim)
  modRAW_allF <- predict(krillmaxMods_allF[[j]], neClim, args='outputformat=raw')
  krillmaxStack_allF <- stack(krillmaxStack_allF, mod_allF)
  krillmaxStackRAW_allF <- stack(krillmaxStackRAW_allF, modRAW_allF)
}

#plot the output (only one rep, so no mean or standard deviation)
plot(krillmaxStack_allF, col=rgb.tables(1000))

# MODEL 3 -----------------------------------------------------------------
#### MAXENT - LINEAR FEATURES + SAMPLING BIAS
# Account for smapling bias, linear features only
# 10,000 locations selected using probabilistic target-group sampling
# from KDE bias surface created above

# example of selecting background points
# to reflect bias in the presence data
# BG points are more likely to be selected where
# presence-only sampling is most dense

#this code creastes a raster surface with the underlying
#sampling bias estimated to account for sampling bias

bg <- KDEpts[sample(seq(1:nrow(KDEpts)), 
                    size=1000, 
                    replace=T, 
                    prob=KDEpts[,"layer"]),1:2]
plot(neClim[[1]])
points(bg, pch=20)

krillmaxMods_LF_bias <- list()
for(f in 1:sets){
  print(f)
  krillmaxMods_LF_bias[[f]] <- maxent(neClim, 
                                    krillTrain[[f]],
                                    # background points selected from KDEpts
                                    a=KDEpts[sample(seq(1:nrow(KDEpts)), 
                                                    size=10000, 
                                                    replace=T, 
                                                    prob=KDEpts[,"layer"]),1:2], 
                                    args=c(c("-h", "-q", "-p", "-P", "nothreshold"),c("-m", 10000)))
}

# Predict to geography and stack 
krillmaxStack_LF_bias <- predict(krillmaxMods_LF_bias[[1]], neClim)
krillmaxStackRAW_LF_bias <- predict(krillmaxMods_LF_bias[[1]], neClim, args='outputformat=raw')

for(j in 2:sets){
  print(j)
  mod_LF_bias <- predict(krillmaxMods_LF_bias[[j]], neClim)
  modRAW_LF_bias <- predict(krillmaxMods_LF_bias[[j]], neClim, args='outputformat=raw')
  krillmaxStack_LF_bias <- stack(krillmaxStack_LF_bias, mod_LF_bias)
  krillmaxStackRAW_LF_bias <- stack(krillmaxStackRAW_LF_bias, modRAW_LF_bias)
}


plot(krillmaxStack_LF_bias, col=rgb.tables(1000))
plot(mean(krillmaxStack_LF_bias), col=rgb.tables(1000)) # mean of the five models
plot(calc(krillmaxStack_LF_bias, sd), col=rgb.tables(1000)) #std dev of the five models


# MODEL 4 -----------------------------------------------------------------
#### MAXENT - DEFAULT FEATURES - BIAS CORRECTED BACKGROUND
krillmaxMods_allF_bias <- list()
for(f in 1:sets){
  print(f)
  krillmaxMods_allF_bias[[f]] <- maxent(neClim, 
                                      krillTrain[[f]], 
                                      a=KDEpts[sample(seq(1:nrow(KDEpts)), 
                                                      size=10000, 
                                                      replace=T, 
                                                      prob=KDEpts[,"layer"]),1:2], 
                                      args=c("-P",c("-m", 10000)))
}

# Predict to geography and stack
krillmaxStack_allF_bias <- predict(krillmaxMods_allF_bias[[1]], neClim)
krillmaxStackRAW_allF_bias <- predict(krillmaxMods_allF_bias[[1]], neClim, 
                                    args='outputformat=raw')
for(j in 2:sets){
  print(j)
  mod_allF_bias <- predict(krillmaxMods_allF_bias[[j]], neClim)
  modRAW_allF_bias <- predict(krillmaxMods_allF_bias[[j]], neClim, 
                              args='outputformat=raw')
  krillmaxStack_allF_bias <- stack(krillmaxStack_allF_bias, mod_allF_bias)
  krillmaxStackRAW_allF_bias <- stack(krillmaxStackRAW_allF_bias, modRAW_allF_bias)
}

plot(krillmaxStack_allF_bias, col=rgb.tables(1000))
plot(mean(krillmaxStack_allF_bias), col=rgb.tables(1000)) # mean of the five models
plot(calc(krillmaxStack_allF_bias, sd), col=rgb.tables(1000)) #std dev of the five models

#save workspace image
save.image(file=paste(krillSpp[a], ".RData", sep=""))
#}
################################################################################


# Next, we will calculate mean and standard deviation (sd) rasters for all the 
# models fitted above and save the prediction rasters to disk
################################################################################
# CHUNK 3: Write mean and sd rasters of predictions for evaluation, mapping, etc
################################################################################

#krillFile <- "C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/MaxEnt/krill.RData"
#load(krillFile)

fileName <- "krill"

#maxent - LINEAR FEATURES
writeRaster(sum(krillmaxStack_LF)/dim(krillmaxStack_LF)[3], 
            paste0(fileName, "_maxent_prob_LF.tiff"), 
            type="GTiff", overwrite=T)
writeRaster(calc(krillmaxStack_LF, sd), 
            paste0(fileName, "_maxent_sd_LF.tiff",sep=""), 
            type="GTiff", overwrite=T)  

#maxent - ALL FEATURES
writeRaster(sum(krillmaxStack_allF)/dim(krillmaxStack_allF)[3], 
            paste0(fileName, "_maxent_prob_allF.tiff"), 
            type="GTiff", overwrite=T)
writeRaster(calc(krillmaxStack_allF, sd), 
            paste0(fileName, "_maxent_sd_allF.tiff",sep=""), 
            type="GTiff", overwrite=T)

#maxent - LINEAR FEATURES - ***bias corrected***
writeRaster(sum(krillmaxStack_LF_bias)/dim(krillmaxStack_LF_bias)[3], 
            paste0(fileName,"_maxent_prob_LF_biasCorrected.tiff"), 
            type="GTiff", overwrite=T)
writeRaster(calc(krillmaxStack_LF_bias, sd), 
            paste0(fileName,"_maxent_sd_LF_biasCorrected.tiff"), 
            type="GTiff", overwrite=T)  

#maxent - ALL FEATURES - ***bias corrected***
writeRaster(sum(krillmaxStack_allF_bias)/dim(krillmaxStack_allF_bias)[3], 
            paste0(fileName, "_maxent_prob_allF_biasCorrected.tiff"), 
            type="GTiff", overwrite=T)
writeRaster(calc(krillmaxStack_allF_bias, sd), 
            paste0(fileName,"_maxent_sd_allF_biasCorrected.tiff"), 
            type="GTiff", 
            overwrite=T)
################################################################################



################################################################################
# CHUNK 4: Calculate AICc for maxent models
################################################################################
library(PresenceAbsence)
library(raster)
library(dismo)
library(zoo)
library(ENMeval)

#work with the outdated ENMeval functions
library("remotes")
library("fs")


#create separate libraries for housing
dev_lib <- path_home_r("R/win-library/dev-versions/")
old_lib <- path_home_r("R/win-library/old-versions/")

#detach current version
detach("package:ENMeval")

install_version("ENMeval", version = "0.2.0", lib = old_lib)
library("ENMeval")
packageVersion("ENMeval")


#krillFile <- "C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/MaxEnt/krill.RData"
#load(krillFile)

aicc.LF <- aicc.allF <- aicc.LF_bias <- aicc.allF_bias <- NULL

#calc.aicc() is now aic.maxent(), so use ENMeval version 0.2.0
for(ii in 1:5){
  aicc.LF[ii] <- calc.aicc(nparam = get.params(krillmaxMods_LF[[ii]]), 
                           occ=krillGeoXY, 
                           krillmaxStack_LF[[ii]])$AICc
  
  #aicc.allF[ii] <- calc.aicc(nparam = get.params(krillmaxMods_allF[[ii]]), 
                             #occ=krillGeoXY, 
                             #krillmaxStack_allF[[ii]])$AICc
  
  aicc.LF_bias[ii] <- calc.aicc(nparam = get.params(krillmaxMods_LF_bias[[ii]]), 
                                occ=krillGeoXY, 
                                krillmaxStack_LF_bias[[ii]])$AICc
  
  aicc.allF_bias[ii] <- calc.aicc(nparam = get.params(krillmaxMods_allF_bias[[ii]]), 
                                  occ=krillGeoXY, 
                                  krillmaxStack_allF_bias[[ii]])$AICc
}    

meanaicc.LF <- mean(as.numeric(aicc.LF[1]),as.numeric(aicc.LF[2]), as.numeric(aicc.LF[3]), as.numeric(aicc.LF[4]), as.numeric(aicc.LF[5]))
meanaicc.LF_bias <- mean(as.numeric(aicc.LF_bias[1]),as.numeric(aicc.LF_bias[2]), as.numeric(aicc.LF_bias[3]), as.numeric(aicc.LF_bias[4]), as.numeric(aicc.LF_bias[5]))
meanaicc.allF_bias <- mean(as.numeric(aicc.allF_bias[1]),as.numeric(aicc.allF_bias[2]), as.numeric(aicc.allF_bias[3]), as.numeric(aicc.allF_bias[4]), as.numeric(aicc.allF_bias[5]))



################################################################################

#evaluate variables and collinearity
library(usdm)
vifCovariates <- as.character(vifstep(neClim)@results$Variables)
bioRastsKeep <- neClim[[vifCovariates]]
plot(bioRastsKeep, col=rgb.tables(1000), box=F, legend=T, axes=T)

################################################################################
# CHUNK 5: Evaluate models
################################################################################
library(PresenceAbsence)
library(raster)
library(dismo)
library(zoo)
library(ecospat)


######### maxent LF ############# 
Boyce <- AUCmod <- meanProb <- meanBG <- NULL

# predicted probability at random background points
probBG <- extract(krillmaxStack_LF, randomPoints(neClim, 10000))
probBG <- extract(krillmaxStack_LF, x)

for(ii in 1:dim(krillmaxStack_LF)[3]){    
  probTest <- as.numeric(na.omit(extract(krillmaxStack_LF[[ii]], krillTest[[ii]])))
  # predicted probability at test points
  Boyce[[ii]] <- ecospat.boyce(krillmaxStack_LF[[ii]], krillTest[[ii]],
                               PEplot=FALSE)$Spearman.cor
  evalDismo <- evaluate(p=probTest, a=probBG[,ii])
  AUCmod[[ii]] <- evalDismo@auc
  meanProb[[ii]] <- mean(probTest)
  meanBG[[ii]] <- mean(probBG[,ii])
}

maxentEval_LF <- rbind(Boyce, AUCmod, meanProb, meanBG)

######### maxent allF #############  
Boyce <- AUCmod <- meanProb <- meanBG <- NULL

# predicted probability at random background points
probBG <- extract(krillmaxStack_allF, randomPoints(neClim, 10000))

for(ii in 1:dim(krillmaxStack_allF)[3]){    
  
  probTest <- as.numeric(na.omit(extract(krillmaxStack_allF[[ii]], krillTest[[ii]])))
  
  # predicted probability at test points
  Boyce[[ii]] <- ecospat.boyce(krillmaxStack_allF[[ii]], krillTest[[ii]],
                               PEplot=FALSE)$Spearman.cor
  evalDismo <- evaluate(p=probTest, a=probBG[,ii])
  AUCmod[[ii]] <- evalDismo@auc
  meanProb[[ii]] <- mean(probTest)
  meanBG[[ii]] <- mean(probBG[,ii])
}

maxentEval_allF <- rbind(Boyce, AUCmod, meanProb, meanBG)

######### maxent LF bias corrected ############# 
Boyce <- AUCmod <- meanProb <- meanBG <- NULL

# predicted probability at random background points
probBG <- extract(krillmaxStack_LF_bias, randomPoints(neClim, 10000))

for(ii in 1:dim(krillmaxStack_LF_bias)[3]){    
  
  probTest <- as.numeric(na.omit(extract(krillmaxStack_LF_bias[[ii]], krillTest[[ii]])))
  
  # predicted probability at test points
  Boyce[[ii]] <- ecospat.boyce(krillmaxStack_LF_bias[[ii]], krillTest[[ii]],
                               PEplot=FALSE)$Spearman.cor
  evalDismo <- evaluate(p=probTest, a=probBG[,ii])
  AUCmod[[ii]] <- evalDismo@auc
  meanProb[[ii]] <- mean(probTest)
  meanBG[[ii]] <- mean(probBG[,ii])
}

maxentEval_LF_bias <- rbind(Boyce, AUCmod, meanProb, meanBG)

######### maxent allF bias corrected #############  
Boyce <- AUCmod <- meanProb <- meanBG <- NULL

# predicted probability at random background points
probBG <- extract(krillmaxStack_allF_bias, randomPoints(neClim, 10000))

for(ii in 1:dim(krillmaxStack_allF_bias)[3]){    
  
  probTest <- as.numeric(na.omit(extract(krillmaxStack_allF_bias[[ii]], krillTest[[ii]])))
  
  # predicted probability at test points
  Boyce[[ii]] <- ecospat.boyce(krillmaxStack_allF_bias[[ii]], krillTest[[ii]],
                               PEplot=FALSE)$Spearman.cor
  evalDismo <- evaluate(p=probTest, a=probBG[,ii])
  AUCmod[[ii]] <- evalDismo@auc
  meanProb[[ii]] <- mean(probTest)
  meanBG[[ii]] <- mean(probBG[,ii])
}

maxentEval_allF_bias <- rbind(Boyce, AUCmod, meanProb, meanBG)

rowMeans(maxentEval_LF)
rowMeans(maxentEval_allF)
rowMeans(maxentEval_LF_bias)
rowMeans(maxentEval_allF_bias)
################################################################################
