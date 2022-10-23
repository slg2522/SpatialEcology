###Assignment: Like many analyses in spatial ecology, working with SDMs requires that you have skills to
###prepare the necessary spatial data sets. In this HW assignment, we will be fitting SDMs to our good friend
###the Austral grass tree (Xanthorrhoea australis). To do so, we will need to assemble two data sets: (1) species
###occurrence records and (2) environmental (bioclimatic) rasters. You will first prepare the bioclimatic rasters
###(from Worldclim) as in HW #2 (but using a different subset of variables as outlined below). You will then
###download (from GBIF) and prepare a thoroughly cleaned set of occurrence records for X. australis. We will
###divide the occurrence data into training and testing sets for model fitting (training) and evaluation (testing)
###and will check our candidate variables for potential issues with collinearity, removing any variables that are
###problematic before fitting models.

###Data ‘cleaning’ is always critical and especially so for data downloaded from online biodiversity databases
###such as GBIF. Data cleaning typically involves removing duplicates, erroneous records (i.e., records with
###wrong geographic coordinates, outside of the native range of the species, low spatial precision, etc.). The
###goal is to produce a data set that is appropriate for (1) fitting SDMs (i.e., to avoid the old modeling adage:
###‘garbage in, garbage out’) and (2) your particular research objective. One important step in the data cleaning
###process is plotting your data to make sure everything overlaps correctly in geographic space and generally
###makes sense. The necessary steps from HW #2 to prepare the data are repeated below, some with minor
###modifications, so read carefully. Refer to the HW #2 solution to make sure your code works correctly.

###As always, you will be graded on your ability to produce clean, well commented R code that performs the
###tasks listed below without error. When you are done, push your code to GitHub, following the instructions
###provided in the document: mees698C.submittingHW.pdf.
###Let’s get started!
  
###1. To reduce computational demands, we will be using rasters with a coarser resolution (grain) than we
###used in HW #2. Use functions in the geodata package to download the Worldclim bioclimatic variables
###at 5 arc-minute resolution. Note that the resulting object will be of class terra::SpatRaster and I
###found it easier to convert it to class raster::stack and to also rename the layers. See HW #2 solution
###for how to do this if you are unsure.

# download the bioclim variables
library(geodata)
bioRasts <- worldclim_global(var="bio",
                             res=5,
                             path=getwd())

class(bioRasts) # SpatRaster
bioRasts <- raster::stack(bioRasts)
class(bioRasts) # raster stack
# update the layer names
names(bioRasts) <- paste0("bio", 1:19)


###2. In HW #2, we worked with four of the bioclimatic variables. Here, we want to consider more candidate
###variables:
###• Modify the raster stack of the 19 bioclim variables downloaded in step #1 to produce a new stack
###that contains all variables EXCEPT bio1, bio8, bio9, bio12, bio13, bio14, bio16, and bio17
###(in other words, retain bio2-7, 10, 11, 15, 18, and 19).
###• Crop the resulting raster stack to the outline of Australia (not the extent) using the shapefile
###provided with HW #2. Rename the cropped rasters so they have the correct names (bio2, bio3,
###etc.). Depending on how you go about masking / cropping the rasters, you may end up with
###a raster with a global extent & many ros and columns of NA values. You may need to use the
###raster::trim function to remove these extra rows / columns. The trim function can be slow, so
###be patient.

library(raster)
# keep the necesssary rasters bio2-7, 10, 11, 15, 18, and 19
bioRasts <- bioRasts[[c("bio2", "bio3", "bio4", "bio5", "bio6", "bio7",  "bio10", "bio11", "bio15", "bio18", "bio19")]]
# load shapefile # plot
aus_nz <- shapefile("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/Homework5/oz_nz_aea.shp")
plot(aus_nz)

#subset australia out
aus <- subset(aus_nz, NAME=="Australia")
plot(aus)

#check if rasters are in the same projection
projection(aus)
projection(bioRasts)

# project the vector data
ausProj <- spTransform(aus, CRSobj = projection(bioRasts)) 

# create a mask raster to use for clipping
ausRast <- raster(ausProj, res=res(bioRasts))
ausRast <- setValues(ausRast, 1)
ausRast <- mask(ausRast, ausProj)

origin(ausRast)
origin(bioRasts)
origin(ausRast) <- origin(bioRasts)
# now we clip by multiplying the mask and the bioRasts together
bioRasts <- bioRasts*ausRast
names(bioRasts) <- c("bio2", "bio3", "bio4", "bio5", "bio6", "bio7",  "bio10", "bio11", "bio15", "bio18", "bio19")

#trim the rasterbrick
bioRasts <- trim(bioRasts, values=NA)

###3. Next, obtain species occurrence records for the Austral grass tree (Xanthorrhoea australis). As in HW
### #2, use the gbif function in the dismo package. Once downloaded from GBIF, clean the resulting
###data frame by removing records that:
###• Do not have geographic coordinates
###• Fall outside the native range of the species (southeast corner of Australia only)
###• Do not overlap the bioclimatic rasters
###• Have coordinate uncertainty greater than 10 km
###• Are duplicated
##• Are spatial duplicates

###What are spatial duplicates? Spatial duplicates are observations that are close enough in geographic space
###such that they fall in the same raster grid cell, so spatial duplication depends on the resolution of the raster
###data. We generally do not want to fit a model using spatial duplicates.

library(dismo)
# download from GBIF
grassTree <- gbif(genus="Xanthorrhoea", species = "australis", geo=T)
# select columns:
#acceptedScientificName, lon, lat, coordinateUncertaintyInMeters
grassTree <- grassTree[,c("acceptedScientificName", "lon", 
                          "lat",  "coordinateUncertaintyInMeters")]
# remove all NA's
grassTree <- na.omit(grassTree)

# convert to a spatial object and a
coordinates(grassTree) <- c("lon", "lat")
projection(grassTree) <- projection(ausProj)

#remove non-Australia values
ausGrassTree <- over(grassTree, ausProj) # overlay
ausGrassTree <- which(is.na(ausGrassTree)) # get records that are NA
grassTree <- grassTree[-ausGrassTree,] # remove NA records

#remove the duplicates
#get the cell number of each record
cells <- cellFromXY(bioRasts, grassTree)
# use the duplicated function to find duplicate cell numbers
dups <- duplicated(cells) # returns 'TRUE' if the cell number is duplicated
grassTreeND <- grassTree[!dups,] # the ! = not, so here we keep dups == FALSE
shapefile(grassTreeND, "grassTree.shp", overwrite=T) # save as shapefile

#check for species-- only one species
unique(grassTree$acceptedScientificName)

###HW QUESTION: In general terms, how would you expect the resolution of a raster to influence
###the number of spatial duplicates?

#I would expect that the coarser the resolution of the raster, the more points would be considered spatial
#duplicates because as the resolution of a raster goes up, the cell size gets bigger and bigger. A larger
#cell will have more area and thus more likelihood to have multiple points within it. A really fine
#resolution raster will have a bunch of small cells, so it is more liekly that points will have their
#own cells and thus there will be less spatial duplication.
  
  
###Removing spatial duplicates will be easier after you have converted the GBIF data into a SpatialPointsDataFrame.
###At the end of step #3, you should have a cleaned point occurrence data set with the correct CRS and
###containing only these attributes: acceptedScientificName, lon, lat, coordinateUncertaintyInMeters.
###Be sure to plot your data to check if everything seems OK. I ended up with about 730 points post-cleaning
###(Fig. 2). You may have a few more or less points, but your result should be close to this number.

# transform the grass tree data from WGS84
grassTreeProj <- spTransform(grassTreeND, CRSobj = projection(aus))
bio10 <- bioRasts$bio10 # make a bio10 raster layer
bio10Proj <- projectRaster(bio10, crs=projection(aus)) # transform
writeRaster(bio10Proj, "bio10Proj.tif", overwrite=T) # save raster as GeoTiff

# use colorRamps for the rainbow 
library(colorRamps)
plot(bio10Proj, col=rgb.tables(1000), alpha=0.5) # alpha  sets transparency
plot(aus, add=T) # add the polygon
# add the points with the color order set to the year column
#pch=21 is a circle symbol
points(grassTreeProj, pch=21, lwd=1, cex=0.8, 
       bg="black")

###After Step #3, you should have (1) a prepared set of bioclimatic rasters and (2) cleaned presence-only
###occurrence records. We need to do a few more things before we are ready to fit and evaluate models. First,
###we need to remove highly correlated variables and then divide the occurrence data set into training and
###testing sets. We will be fitting two presence-only SDM methods (Mahalanobis and Maxent). However, our
###evaluation metrics require absence data and so we will need to generate background points (pseudo-absences)
###for the model evaluation / testing.

###Let’s start with checking variable correlations. To do so, you will need to extract the values of the rasters at
###the occurrence records first (same as in HW #2 - see solution if you are unsure).

#extract the values of rasters where tree is
sppDat <- data.frame(extract(bioRasts, grassTreeND))
sppDat <- na.omit(sppDat)
head(sppDat)
                              
###4. Use your cleaned point occurrence data to extract the bioclimatic variables from the raster stack. You
###should end up with a table similar to this (only first few rows printed):

#view dataframe header
head(sppDat)
#matches the example shown

###5. Use the vifstep function in the usdm library to remove highly correlated
#variables from the raster stack. Create a new raster stack containing only the
#variables retained by vifstep. You will use this new raster stack to fit SDMs.
#I ended up with 6 uncorrelated bioclimatic variables - 3 temperature variables
#and 3 precipitation variables

library(usdm)
vifstep(bioRasts)
#Vifstep Output:
#5 variables from the 11 input variables have collinearity problem: 
#  
#  bio7 bio11 bio6 bio5 bio4 
#
#After excluding the collinear variables, the linear correlation coefficients ranges between: 
#  min correlation ( bio10 ~ bio3 ):  0.0375415 
#max correlation ( bio19 ~ bio10 ):  -0.7668125 
#
#---------- VIFs of the remained variables -------- 
#  Variables      VIF
#1      bio2 2.827502
#2      bio3 3.333292
#3     bio10 8.085931
#4     bio15 6.571798
#5     bio18 2.703522
#6     bio19 3.318741

#new raster stack containing only the variables retained by vifstep
bioRastsUC <- bioRasts[[c("bio2", "bio3", "bio10", "bio15", "bio18", "bio19")]]


###Now we need to (1) divide the occurrence data into training-testing sets and (2) create a set of background
###points for model evaluation.
                              
###6. Divide the presence-only data table into 80% training and 20% testing data sets (see ?kfold). Make a
###plot showing the training and testing data as different symbols.

#Create training/test datasets
#allow to be replicated
set.seed(0)
#set presence
library(dplyr)
presvals <- select(sppDat,"bio2","bio3", "bio10", "bio15", "bio18", "bio19")
head(presvals)
# create 10,000 random background points
backgr <- randomPoints(bioRastsUC, 10000)
# and then extract env data at the background points
absvals <- extract(bioRastsUC, backgr)
# make a vector of 1's and 0's to match the
# presence records and the background data

#cheat and create background data
# See ?rep
sppPA <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
# now we bind everything together into a data.frame
sdmData <- data.frame(cbind(sppPA, rbind(presvals, absvals)))
# and have a look at the data
head(sdmData)
summary(sdmData) #10603 rows

# for plotting, extracting, etc
grassTreeDF <- as.data.frame(grassTreeND)
grassTreeDF <- grassTreeDF[,2:3]
names(grassTreeDF) <- c("x", "y")
grassTreeND <- grassTreeDF[1:10745,]
ptsXY <- rbind(grassTreeND, backgr) #20245 rows
n <- max(nrow(ptsXY))-max(nrow(sdmData)) 
sdmData[nrow(sdmData) + n,] = c(NA, NA, NA, NA, NA, NA, NA)
sdmData <- cbind(ptsXY, sdmData)
head(sdmData)

# 5 groups = 80/20 split (each group has 20% of the data)
group <- kfold(sdmData, k=5)

#4 groups for training (80%)
sdmTrain <- sdmData[group != 1, ]
#1 group for testing (20%)
sdmTest <- sdmData[group == 1, ]

#split the data up with 80/20
presTrain <- sdmTrain[sdmTrain$sppPA==1,c("x", "y")]
bgTrain <- sdmTrain[sdmTrain$sppPA==0,c("x", "y")]
presTest <- sdmTest[sdmTest$sppPA==1,c("x", "y")]
bgTest <- sdmTest[sdmTest$sppPA==0,c("x", "y")] 

# lets plot the data of the training testing datasets
#use different symbols for each
plot(bioRastsUC[[1]], col='light grey', legend=FALSE)
plot(aus, add=T)
# presence pts (training)
points(presTrain, pch= '+', col='red') 
# presence pts (testing)
points(presTest, pch='o', col='blue')
# background pts (training)
points(bgTrain, pch="*", col='yellow')
# background pts (testing)
points(bgTest, pch="x", col='black')


###7. Create a spatialPoints object containing 10,000 random background (pseudo-absence) points. See
###randomPoints in the dismo package.

#created above with the code:
#Create training/test datasets
#allow to be replicated
#set.seed(0)
# create 10,000 random background points
#backgr <- randomPoints(bioRastsUC, 10000)
# and then extract env data at the background points
#absvals <- extract(bioRastsUC, backgr)
# make a vector of 1's and 0's to match the
# presence records and the background data


###8. Use your training data and the uncorrelated set of bioclimatic rasters to fit and predict a Mahalanobis
###model (using the mahal function in dismo).


#Mahalanobis Distance
mm <- mahal(stack(bioRastsUC), # raster stack
            presTrain) #presence-only data

# predict the distribution
pm <- predict(stack(bioRastsUC), # raster stack
              mm, # model
              progress='text')
plot(pm) # predictions are 1-distance
#get some really large negative distances, so convert to probabilities


###9. Make a map of the prediction (note that mahal predictions are slow, so it may take a few minutes
###to complete this step). The predictions from mahal are 1-distance, so we will need to convert the
###distance predictions from the mahal function to a probability.
###Here is some R code to do that - it takes as input (1) a raster of the raw prediction (called mahalPred in the
###code below) from the predict function and the stack of rasters used to fit the model (called bioRastsKeep
### Convert distances to a p-value
### Mahal distances (Dˆ2) are Chi-square distributed
###probMap <- (1-mahalPred)
###dists <- as.numeric(na.omit(getValues(probMap)))
###p.value <- 1-as.numeric(pchisq(dists, df=nlayers(bioRastsKeep)))
###probMap[!is.na(probMap[])] <- p.value

# let's convert to a p-value
# Mahal distances (D^2) are Chi-square distributed
probMap <- (1-pm)
#plot probability map
plot(probMap)
#p-value calculations
dists <- as.numeric(na.omit(values(probMap)))
p.value <- 1-as.numeric(pchisq(dists, df=nlayers(bioRastsUC)))
probMap[!is.na(probMap[])] <- p.value

# evaluate the model using test data (presences + background)
e <- evaluate(p=extract(probMap, presTest), # presences
              a=extract(probMap, bgTest)) # background / absences
e

#what am I calling habitat vs waht am I not calling habitat
#plot it
par(mfrow=c(1,2))
plot(probMap, main='Mahalanobis distance (p-value)')
plot(wrld_simpl, add=TRUE, border='dark grey')
tr <- threshold(e, # model eval object
                'no_omission')
plot(probMap > tr, main='presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey')
points(sdmTrain[sdmTrain$sppPA==1,c("x", "y")], pch='+')
points(sdmTest[sdmTest$sppPA==1,c("x", "y")], pch='x', col="red")


###10. Use your training occurrence data and the uncorrelated set of bioclimatic rasters to fit and predict a
###MaxEnt model (using the maxent function in dismo). Use jackknife to assess variable importance.
###Make a map of the prediction.

maxent()
#make sure that you set your filepath bc this has a lot of data
filePath <- "C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/Homework5"

#measure variable importance using jackknife and produce response curves
mx <- maxent(stack(bioRastsUC), 
             presTrain,
             path=filePath,
             args=c("jackknife", "responsecurves"))

plot(mx)
mx

# evaluate the model using the test data
e <- evaluate(presTest, bgTest, mx, bioRastsUC)
e

# predict back to geography
mxPred <- predict(mx, bioRastsUC)
plot(mxPred, col=rgb.tables(1000), main="Maxent Prediction")
# Predict in 'raw' format and save raster of prediction
mxPred <- predict(mx, bioRastsUC, args=c("outputformat=raw"),
                  filename=paste0(filePath, '/maxent_predictionJacknife.tif'))


#check model quality using the Boyce Index
predRast <- raster(paste0(filePath, '/maxent_predictionRAW.tif'))
ecospat.boyce(predRast, presTest)

###HW QUESTION: What are the top two most important variables associated with the
###distribution of the Austral grass tree? Which variable is least important?

#According to the Analysis of Variable Contributions Table, bio19 contributes
#71.4% with 66.5% permutation importance abd bio10 contributes 19.2% with 6.3%
#permutation importance. The least important variable is bio15, which contributes
#1.1% and 7.6 permutation importance.

#The Jacknife of Regularized Training Gain for Species shows bio10 as the most 
#important (accounting for about 1.9 regularized training gain with only variable
#and 2.1 without the variable). Bio19 is the seond-most important, reporting ~1.8
#regularized training gain with only this variable and ~2.05 without the variable.
#The jacknife disagrees with the Analysis of Variable Contributions Table for the
#least important variable, and reports bio18 as 0.18 with only the variable and
#about 2.1 without the variable.

#Overall, bio10 is the environmental variable with highest gain when used
#alone, which therefore likely holds the most useful information alone. 
#Bio19 decreases the gain the most when it is omitted, likely making it 
#contain the most information that cannot be captured from the other variables.


###HW QUESTION: Compare the predicted distributions from the two SDMs. How are they
###similar / different? Where do the models over- or under-predict the distribution? What might
###account for these model errors?

#Mahalanobis Plot
plot(probMap, main='Mahalanobis distance (p-value)')
#Maxent Plot
plot(mxPred, col=rgb.tables(1000), main="Maxent Prediction")

#The Mahalanobis Distance plot dramatically over predicts the distribution of trees
#suggesting their range expands from the east coast of Australia to central,
#southern, and western portions of Australia. This prediction is likely driven
#by the large amount of presence-only data points concentrated along the east
#coast combined with several rarer and more central points. Since Mahalanobis
#distance measures the relative distance to a central point representing the
#overall mean, the presence of multivariate outliers may have skewed the centroid
#further westward than would be anticipated had they been removed ahead of analysis.
#This problem may have been exacerbated by high correlation in the environmental
#variables, which despite undergoing cleaning, were likely related (as we only
#examined temperature and precipitation). When the variables are highly correlated,
#this interferes with the precision of the inverse of the correlation matrix, an 
#element essential for model calculations. As a result, Egan & Morgan, 1998;
#Hadi & Simonoff, 1993; and Varmuza & Filzmoser, 2016 caution against using the 
#approach.

#The Maxent predictions are similar to the Mahalanobis Distance in that they both
#predict tree presence on the east coast of Australia, however, the maxent
#predictions are much more spatially conservative than the former. The Maxent
#model predicts tree populations along the east coast of Australia similar to the
#current range and on the southeast most coastal portion of Australia. This 
#prediction assumes relatively low geographic expansion (in some plaaces more 
#constrained spatial ranges than are currently occupied) and likely underestimates 
#the potential for range shifts. The maxent prediction may be overfitted as a 
#result of either large sample size in small area, user selected choices, the
#provided background points were too narrow, sampling bias to the west coast, 
#region mismatch between background samples and extrapolation to the new environment,
#or improper regularization leading to a highly complex model.


###11. Evaluate the mahal and maxent models using the testing data and background data.

#steps completed as part of the processes above. See code:
## evaluate the model using test data (presences + background)
#e <- evaluate(p=extract(probMap, presTest), # presences
              #a=extract(probMap, bgTest)) # background / absences
#e

#and:

# evaluate the model using the test data
#e <- evaluate(presTest, bgTest, mx, bioRastsUC)
#e


###HW QUESTION: Briefly discuss the model evaluation metrics. Which model performed best?
###My AUC values for these models were quite similar even though their predictions were not. If
###you were a conservation manager and were provided output from these two models, how might
###you handle this seeming contradiction between the differences in the spatial predictions, but
###similarity in AUC?

#Mahal:
#class          : ModelEvaluation 
#n presences    : 141 
#n absences     : 4 
#AUC            : 0.714539 
#cor            : -0.01406406 
#max TPR+TNR at : 0.9948285

#Maxent:
#class          : ModelEvaluation 
#n presences    : 141 
#n absences     : 4 
#AUC            : 0.3031915 
#cor            : -0.0901167 
#max TPR+TNR at : 0.4357871

###Which model performed best?
#According to the AUC values, the Mahalanobis Distance performed better than the Maxent.
#However, the AUC is a problematic assessment method, as the technique
#requires presence and absence data, but only presence data are supplied. AIC is 
#the preferred and more accurate method for assessing Maxent models' abilities to
#distinguish between presences from background location potential presences. In
#addition, comparisons between models require the same landscape, background locations,
#species, and test data. In the case of this exercise, the randomized test data and 
#training sets should be statistically similar, but are not necessary equivalent.
#Further, the assessment requires species to be near range equilibrium and it is
# unclear whether this is the case.

###If you were a conservation manager and were provided output from these two models, how might
###you handle this seeming contradiction between the differences in the spatial predictions, but
###similarity in AUC?
#The results should be interpreted with full acknowledgement of the above criteria.
#That being said, as a conservation manager I would want to run a variety of models
#and assessment criteria, knowing that each method has downfalls. By comparing
#where the ouputs share prediction areas and prioritize variables, a pattern
#of likely range exapnsion/contraction, probable locations, and governing
#variables may emerge. While these two models differ substantially in their 
#predictions, other models may fall more in the middle of the spectrum. Further,
#as a conservation manager, I would want to supply more environmental information
#in case there are more or equally important variables governing the range.


###HW QUESTION: How might you improve these models?
#Mahalanobis distance rapidly deteriorates as sample size decreases. In the case
#of more available samples, a simple fix would be to provide the model with more 
#input, but this is not always feasible. To simulate a near infinite sample size 
#without providing numerous samples, the algorithm could in theory be improved
#by using an expectation-maximization component, such that each principal component's
#score's bias is mitigated by dividing by some numerical expectation. This method 
#would act similarly to adding more datapoints in a high presence density area and 
#effectively dwarfing low presence density outliers.

#Maxent suffers from a propensity for model fitting, high sensitivity to evaluation
#data, and model overcomplexity. To avoid these tendencies, I would recommend
#substituting the current random partitionment of data, which appears to inflate
#model performance and underestimate range, for a masked geographically structured
#set. This approach would ensure the model covers the current presence range and
#avoids overfitting, with enhanced discriminatory ability over only the potential
#colonization areas. This could be paired with adjustment of the regularization 
#multiplier if it is suspected to be under performing in the discrimination task.
