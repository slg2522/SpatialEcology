# Overview of SDMs in R
library(dismo)
library(maptools)
library(ecospat)
library(corrplot)
library(usdm)
library(colorRamps)

## ---- Load species data-------------------------------------------------------
# csv table with species occurrence data
# csv (Comma separated values) is an easy format to work with
file <- paste0(system.file(package="dismo"), "/ex/bradypus.csv")
file

# read the file
bradypus <- read.csv(file)
# first rows
head(bradypus)
unique(bradypus$species)
# only one species in the data, so
# we only need columns 2 and 3 - the x-y coords:
bradypus <- bradypus[,2:3]
head(bradypus)

#name the coordinates x an y
names(bradypus) <- c("x", "y")

## ---- Load env data-------------------------------------------------------
# file to path to where the rasters are saved
path <- file.path(system.file(package="dismo"), 'ex')
# list the files
files <- list.files(path, pattern='grd$', full.names=TRUE )
files
#there's one variable called biome that is categorical so we will have to deal with the later

# Let's stack the raster files together
# stacking a list of files is fast and easy
predictors <- stack(files) 
names(predictors) # names of the rasters
plot(predictors)

# lets plot the data
data(wrld_simpl)
plot(predictors, 1)
# can also plot by name, try: plot(predictors, "biome")
plot(wrld_simpl, add=TRUE)
# with the points function, "add" is implicit
points(bradypus, cex=0.5, pch=20, col='blue')


## ---- Extract env data-------------------------------------------------------
# Next, we want to extract the environmental values at each
# of the occurrence locations.
# See ?extract
presvals <- extract(predictors, bradypus)
head(presvals)

## ---- Create background (pseudo-absence) data --------------------------------
# setting a random seed to always create the same
# random set of points for this example
set.seed(0)
# create 500 random background points (not thinking at all about constraining them)
backgr <- randomPoints(predictors, 500)
# and then extract env data at the background points
absvals <- extract(predictors, backgr)
# make a vector of 1's and 0's to match the
# presence records and the background data


#cheat and create background data
# See ?rep
sppPA <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
# now we bind everything together into a data.frame
sdmData <- data.frame(cbind(sppPA, rbind(presvals, absvals)))
# biome is a factor, so define it that way
#this allows a categorical variable to be used
sdmData[,'biome'] = as.factor(sdmData[,'biome'])
# and have a look at the data
head(sdmData)
summary(sdmData) #10603 rows

# for plotting, extracting, etc
ptsXY <- rbind(bradypus, backgr) #20245 rows, 2 columns x and y
sdmData <- cbind(ptsXY, sdmData) #10603 rows, 6 columns sppPA, bio2, bio3, bio10, bio15, bio18, bio19
head(sdmData)

# make new stack without the categorical variable 'biome'
# as not all SDMs can use categorical data
pred_nf <- dropLayer(predictors, "biome")


# correlation structure
library(corrplot)
varCor <- cor(na.omit(sdmData[,-c(1:3,12)]))
corrplot(varCor)
ecospat.cor.plot(na.omit(sdmData[,-c(1:3,12)]))

# look at correlation structure, first several variables
# for demonstration purposes
pairs(sdmData[,4:8], cex=0.1)

# clustering with dendrogram
allDistNew <- abs(as.dist(cor(sdmData[,-c(1:3,12)])))
allClusNew <- hclust(1 - allDistNew)
plot(allClusNew, hang=-1)

# Variance Inflation Factor
library(usdm)
vif(sdmData[,-c(1:3,12)])
vifstep(sdmData[,-c(1:3,12)])
vifcor(sdmData[,-c(1:3,12)], th=0.8) 


## ---- Create training / test datasets ----------------------------------------
set.seed(0)
?kfold
group <- kfold(sdmData, k=5) # 5 groups = 80/20 split (each group has 20% of the data)
sdmTrain <- sdmData[group != 1, ] #can pick any number here, this just means that it picks group 1 and leaves 4 groups for testing
sdmTest <- sdmData[group == 1, ]

#split the data up with 80/20
presTrain <- sdmTrain[sdmTrain$sppPA==1,c("x", "y")]
bgTrain <- sdmTrain[sdmTrain$sppPA==0,c("x", "y")]
presTest <- sdmTest[sdmTest$sppPA==1,c("x", "y")]
bgTest <- sdmTest[sdmTest$sppPA==0,c("x", "y")]  

# let's set an extent to crop the env data to make 
# the analyses go a bit faster
ext <- extent(-90, -32, -33, 23)

# lets plot the data of the training testing datasets
plot(pred_nf[[1]], col='light grey', legend=FALSE)
plot(ext, add=TRUE, lwd=2)
# presence pts (training)
points(presTrain, pch= '+', col='red') 
# presence pts (testing)
points(presTest, pch='+', col='blue')
# background pts (training)
points(bgTrain, pch=20, cex=0.5, col='yellow')
# background pts (testing)
points(bgTest, pch=20, cex=0.5, col='black')


## ---- Mahalanobis Distance ---------------------------------------------------
?mahal
mm <- mahal(stack(pred_nf), # raster stack
            presTrain) #presence-only data

# predict the distribution
pm <- predict(stack(pred_nf), # raster stack
              mm, # model
              ext=ext, # ext
              progress='text')
plot(pm) # predictions are 1-distance
#get some really large negative distances, so convert to probabilities

# let's convert to a p-value
# Mahal distances (D^2) are Chi-square distributed
probMap <- (1-pm)
dists <- as.numeric(na.omit(values(probMap)))
p.value <- 1-as.numeric(pchisq(dists, df=nlayers(pred_nf)))
probMap[!is.na(probMap[])] <- p.value

# evaluate the model using test data (presences + background)
?evaluate
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


## ---- Fit MaxEnt -------------------------------------------------------------
maxent()
#make sure that you set your filepath bc this has a lot of data
filePath <- "C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/Week7/Rtutorials"
mx <- maxent(predictors, # env data as a raster stack
             presTrain, # presence data
             factors='biome', # biome is categorical
             path=filePath) # where to save all the output
plot(mx)
mx

# evaluate the model using the test data
e <- evaluate(presTest, bgTest, mx, predictors)
e

# predict back to geography
mxPred <- predict(mx, predictors)
plot(mxPred, col=rgb.tables(1000))
# Predict in 'raw' format and save raster of prediction
mxPred <- predict(mx, predictors, args=c("outputformat=raw"),
                  filename=paste0(filePath, '/maxent_predictionRAW.tif'))

# let's check model quality using the Boyce Index
predRast <- raster(paste0(filePath, '/maxent_predictionRAW.tif'))
ecospat.bo

fyce(predRast, presTest)





## ---- Random Forest -----------------------------------
library(randomForest)
library(biomod2)
library(ggplot2)
# the model formula
model <- sppPA ~ bio1 + bio5 + bio6 + bio7 + bio8 + bio12 + bio16 + bio17 + biome

# fit the RF model
?randomForest # lots of arguments, but let's keep it simple
rf.reg <- randomForest(model, data=sdmTrain, importance=T)
# note the warning - the function assumed regression instead of 
# classification
rf.reg
importance(rf.reg) # variable importance summary


# with pa as a factor to perform classification
model <- factor(sppPA) ~ bio1 + bio5 + bio6 + bio7 + bio8 + bio12 + bio16 + bio17 + biome
rf.class <- randomForest(model, data=sdmTrain, importance=T)
rf.class
importance(rf.class)

# look at variable importance
par(mfrow=c(1,2))
varImpPlot(rf.reg)
varImpPlot(rf.class)
dev.off()

# plot the prediction
par(mfrow=c(1,2))
pHS <- predict(stack(predictors), rf.reg, type="response", ext=ext)
plot(pHS, main='RF: Regression')
plot(wrld_simpl, add=TRUE, border='dark grey')
points(presTrain, pch='+')
points(presTest, pch='+', col="red")

pPA <- predict(stack(predictors), rf.class, type="response", ext=ext)
plot(pPA, main='RF: Classification')
plot(wrld_simpl, add=TRUE, border='dark grey')
points(presTrain, pch='+')
points(presTest, pch='+', col="red")
dev.off()


# evaluate the model
erf.reg <- dismo::evaluate(p=extract(pHS, presTest), 
                           a=extract(pHS, bgTest))
erf.reg 
plot(erf.reg, 'ROC')

# plot response curves
rp <- response.plot2(models = c('rf.class'),
                     Data = sdmTrain[,-c(1:3)],
                     show.variables = c("bio1", "bio12", "bio6", "bio16", "biome"),
                     fixed.var.metric = 'mean', plot = FALSE, 
                     use.formal.names = TRUE)

## define a custom ggplot2 theme
rp.gg.theme <- theme(legend.title = element_blank(),
                     axis.text.x = element_text(angle = 90, vjust = .5),
                     panel.background = element_rect(fill = NA, colour = "gray70"),
                     strip.background = element_rect(fill = NA, colour = "gray70"),
                     panel.grid.major = element_line(colour = "grey90"),
                     legend.key = element_rect(fill = NA, colour = "gray70"))

gg.rp <- ggplot(rp, aes(x = expl.val, y = pred.val, lty = pred.name)) +
  geom_line() + ylab("prob of occ") + xlab("") + 
  rp.gg.theme + 
  facet_grid(~ expl.name, scales = 'free_x')
print(gg.rp)




## ---- Regression Models -------------------------------------------------------
# combine presence-background points
train <- rbind(presTrain, bgTrain)
pb_train <- c(rep(1, nrow(presTrain)), rep(0, nrow(bgTrain)))

#extract env data at training locations
envtrain <- extract(predictors, train)
envtrain <- data.frame( cbind(pa=pb_train, envtrain) )
envtrain[,'biome'] = factor(envtrain[,'biome'], levels=1:14)
head(envtrain)
str(envtrain) # note biome is a factor (categorical)

# prepare testing data
testpres <- data.frame( extract(predictors, presTest) )
testbackg <- data.frame( extract(predictors, bgTest) )
testpres[ ,'biome'] = factor(testpres[ ,'biome'], levels=1:14)
testbackg[ ,'biome'] = factor(testbackg[ ,'biome'], levels=1:14)

## ---- GLM: logistic regression -------------------------------------------------------
# let's fit a simple GLM'
# we need to model formula, family, and link function
glm1 <- glm(formula = pa ~ bio1 + bio12 + bio16 + bio6, 
           family = binomial(link = "logit"), # error distribution and link function
           data=envtrain)
summary(glm1)
coef(glm1)

glm2 <- glm(formula = pa ~ poly(bio12,2)+ bio1:bio12 + bio1,
            family = binomial(link = "logit"), 
           data=envtrain)
summary(glm2)
coef(glm2)

# predict and plot
pg <- predict(predictors, glm1, type="response", ext=ext)
par(mfrow=c(1,3))
plot(pg, main='GLM1')
plot(wrld_simpl, add=TRUE, border='dark grey')


# evaluate the models
ge1 <- dismo::evaluate(testpres, testbackg, glm1, type="response")
ge1
ge2 <- dismo::evaluate(testpres, testbackg, glm2, type="response")
ge2

# let's use a threshold to convert to p/a
tr <- threshold(ge1, 'spec_sens')
plot(pg > tr, main='GLM1: presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey')
points(presTrain, pch='+')
points(bgTrain, pch=20, cex=0.5, col="blue")

pg <- predict(predictors, glm2, type="response", ext=ext)
plot(pg, main='GLM2')
plot(wrld_simpl, add=TRUE, border='dark grey')

# let's plot some response curves
library(ggplot2)
library(biomod2)
rastDat <- na.omit(getValues(predictors))
rp <- response.plot2(models = c('glm1'),
                     Data = envtrain,
                     show.variables = c("bio1", "bio12"),
                     fixed.var.metric = 'mean', plot = FALSE, 
                     use.formal.names = TRUE)
## define a custom ggplot2 theme
rp.gg.theme <- theme(legend.title = element_blank(),
                     axis.text.x = element_text(angle = 90, vjust = .5),
                     panel.background = element_rect(fill = NA, colour = "gray70"),
                     strip.background = element_rect(fill = NA, colour = "gray70"),
                     panel.grid.major = element_line(colour = "grey90"),
                     legend.key = element_rect(fill = NA, colour = "gray70"))
# plot the curves
gg.rp <- ggplot(rp, aes(x = expl.val, y = pred.val, lty = pred.name)) +
  geom_line() + ylab("prob of occ") + xlab("") + 
  rp.gg.theme + 
  facet_grid(~ expl.name, scales = 'free_x')
print(gg.rp)

# variable selection use stepwise methods
library(MASS)
glmStart <- glm(formula = pa ~ bio1 + bio5 + bio6 + bio7 + bio8 + bio12 + bio16 + bio17, 
                family = binomial(link = "logit"), # error distribution and link function
                data=envtrain)
glm.formula <- formula("pa ~ bio1 + bio5 + bio6 + bio7 + bio8 + bio12 + bio16 + bio17")
glm.formula

# implement stepwise using AIC as criteria
glmModAIC <- stepAIC(object=glmStart, 
                     scope=glm.formula,
                     data = envtrain,
                     direction = "both", 
                     trace = 1, 
                     k = 2, 
                     control=glm.control(maxit=500))

# extract variable rankings
anova(glmModAIC)

# plot response curves
rp <- response.plot2(models = c('glmModAIC'),
                     Data = envtrain,
                     show.variables = c("bio1",  "bio6", "bio12"),
                     fixed.var.metric = 'mean', plot = FALSE, use.formal.names = TRUE)

gg.rp <- ggplot(rp, aes(x = expl.val, y = pred.val, lty = pred.name)) +
  geom_line() + ylab("prob of occ") + xlab("") + 
  rp.gg.theme + 
  facet_grid(~ expl.name, scales = 'free_x')
print(gg.rp)



## ---- GAM --------------------------------------------------------------------
if(is.element("package:mgcv", search())) detach("package:mgcv") ## make sure the mgcv package is not loaded to avoid conflicts
library(gam)
# for fitting GAMs, provide:
# (1) model formula, including degree of smoothing
# (2) model family
gam1 <- gam(formula=pa ~ s(bio1,2) + s(bio12,2) + s(bio6,2) + s(bio16,2), 
           data=envtrain, 
           family="binomial")
gam2 <- gam(formula=pa ~ s(bio1,4) + s(bio12,4) + s(bio6,4) + s(bio16,4), 
           data=envtrain, 
           family="binomial")

par(mfrow=c(2,2))
plot(gam1, se=T)

par(mfrow=c(2,2))
plot(gam2, se=T)

# plot the prediction
pg <- predict(predictors, gam2, type="response", ext=ext)
plot(pg, main='GAM1')
plot(wrld_simpl, add=TRUE, border='dark grey')

rp <- response.plot2(models = c('gam1', 'gam2'),
                     Data = envtrain,
                     show.variables = c("bio1", "bio12", "bio6", "bio16"),
                     fixed.var.metric = 'mean', plot = FALSE, use.formal.names = TRUE)

gg.rp <- ggplot(rp, aes(x = expl.val, y = pred.val, lty = pred.name)) +
  geom_line() + ylab("prob of occ") + xlab("") + 
  rp.gg.theme + 
  facet_grid(~ expl.name, scales = 'free_x')
print(gg.rp)
